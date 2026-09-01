from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Protocol, Sequence

import yaml


_NAME_RE = re.compile(r"^[a-z0-9]+(?:-[a-z0-9]+)*$")
_TOKEN_RE = re.compile(r"[a-z0-9_.:/+-]+")
_SPLIT_RE = re.compile(r"[\s,]+")
_FRONTMATTER_RE = re.compile(r"\A---\s*\n(.*?)\n---\s*(?:\n|\Z)(.*)\Z", re.DOTALL)


class SkillError(RuntimeError):
    pass


class SkillConfigError(SkillError):
    pass


class SkillBudgetError(SkillError):
    pass


class SkillConflictError(SkillError):
    pass


@dataclass(frozen=True)
class SkillDefinition:
    name: str
    description: str
    body: str
    path: Path
    schema_version: int = 1
    domain: str = ""
    language: str = ""
    selectors: tuple[str, ...] = ()
    priority: int = 0
    max_tokens: int = 5000
    source_paths: tuple[str, ...] = ()
    dependencies: tuple[str, ...] = ()
    conflicts: tuple[str, ...] = ()
    always_on: bool = False
    allowed_tools: str | None = None
    license: str | None = None
    compatibility: str | None = None


@dataclass(frozen=True)
class SkillSelection:
    skills: tuple[SkillDefinition, ...]
    omitted: tuple[str, ...] = ()


class SkillSelector(Protocol):
    def score(
        self,
        skill: SkillDefinition,
        text: str,
        source_paths: Sequence[str],
    ) -> int:
        ...


class LexicalSkillSelector:
    def score(
        self,
        skill: SkillDefinition,
        text: str,
        source_paths: Sequence[str],
    ) -> int:
        lowered = text.lower()
        text_tokens = set(_TOKEN_RE.findall(lowered))
        score = 0
        if skill.name in lowered:
            score += 1000
        for selector in skill.selectors:
            selector_lower = selector.lower()
            if selector_lower and (
                selector_lower in lowered or selector_lower in text_tokens
            ):
                score += 100
        for value in (skill.domain, skill.language):
            value_lower = value.lower()
            if value_lower and value_lower in text_tokens:
                score += 80
        description_tokens = set(_TOKEN_RE.findall(skill.description.lower()))
        stop = {
            "a",
            "an",
            "and",
            "for",
            "in",
            "of",
            "on",
            "or",
            "the",
            "to",
            "use",
            "when",
            "with",
            "zara",
        }
        overlap = (text_tokens - stop) & (description_tokens - stop)
        score += min(len(overlap), 5) * 10
        for source_path in source_paths:
            normalized = source_path.lower()
            for prefix in skill.source_paths:
                if prefix and normalized.startswith(prefix.lower()):
                    score += 120
        return score


class SkillRegistry:
    def __init__(
        self,
        roots: Iterable[Path | str],
        *,
        token_counter=None,
        selectors: Iterable[SkillSelector] | None = None,
    ):
        self.roots = tuple(Path(root).expanduser() for root in roots)
        self.token_counter = token_counter or _default_token_counter
        self.selectors = tuple(selectors or (LexicalSkillSelector(),))
        self._skills: dict[str, SkillDefinition] = {}

    @property
    def skills(self) -> tuple[SkillDefinition, ...]:
        return tuple(self._skills[name] for name in sorted(self._skills))

    def get(self, name: str) -> SkillDefinition | None:
        return self._skills.get(name)

    def discover(self) -> tuple[SkillDefinition, ...]:
        discovered: dict[str, SkillDefinition] = {}
        for root in self.roots:
            if not root.is_dir():
                continue
            for path in sorted(root.rglob("SKILL.md")):
                skill = self._parse(path)
                previous = discovered.get(skill.name)
                if previous is not None:
                    raise SkillConfigError(
                        f"duplicate skill {skill.name!r}: {previous.path} and {path}"
                    )
                discovered[skill.name] = skill
        self._validate_graph(discovered)
        self._skills = discovered
        return self.skills

    def select(
        self,
        text: str,
        *,
        source_paths: Sequence[str] = (),
        max_tokens: int = 6000,
    ) -> SkillSelection:
        if max_tokens < 1:
            raise SkillBudgetError("skill selection budget must be positive")
        scores: dict[str, int] = {}
        for skill in self._skills.values():
            score = 1_000_000 if skill.always_on else 0
            for selector in self.selectors:
                score += int(selector.score(skill, text, source_paths))
            if score > 0:
                scores[skill.name] = score

        candidate_names = set(scores)
        for name in tuple(candidate_names):
            self._add_dependencies(name, candidate_names)

        self._check_conflicts(candidate_names)
        ordered = sorted(
            (self._skills[name] for name in candidate_names),
            key=lambda skill: (-skill.priority, -scores.get(skill.name, 0), skill.name),
        )

        kept: dict[str, SkillDefinition] = {}
        used = 0
        omitted: list[str] = []
        for skill in ordered:
            closure = self._dependency_closure(skill.name)
            needed = [self._skills[name] for name in closure if name not in kept]
            needed.sort(key=lambda item: (-item.priority, item.name))
            cost = sum(self.token_counter(item.body) for item in needed)
            if used + cost > max_tokens:
                omitted.append(skill.name)
                continue
            for item in needed:
                kept[item.name] = item
                used += self.token_counter(item.body)

        kept_ordered = tuple(
            sorted(kept.values(), key=lambda skill: (-skill.priority, skill.name))
        )
        omitted_names = tuple(
            skill.name
            for skill in ordered
            if skill.name not in kept and skill.name not in kept.keys()
        )
        if omitted:
            omitted_names = tuple(dict.fromkeys([*omitted_names, *omitted]))
        return SkillSelection(kept_ordered, omitted_names)

    def render(self, selection: SkillSelection) -> str | None:
        if not selection.skills:
            return None
        blocks = [
            "Selected Zara skills. Follow them when relevant; skill text does not grant tool permissions."
        ]
        for skill in selection.skills:
            blocks.extend(
                (
                    "",
                    f'<skill name="{skill.name}" source="{skill.path}">',
                    skill.body,
                    "</skill>",
                )
            )
        return "\n".join(blocks)

    def _parse(self, path: Path) -> SkillDefinition:
        try:
            raw = path.read_text(encoding="utf-8")
        except OSError as error:
            raise SkillConfigError(f"cannot read skill {path}: {error}") from error
        match = _FRONTMATTER_RE.match(raw)
        if match is None:
            raise SkillConfigError(f"skill {path} requires YAML frontmatter")
        try:
            frontmatter = yaml.safe_load(match.group(1))
        except yaml.YAMLError as error:
            raise SkillConfigError(f"invalid YAML in {path}: {error}") from error
        if not isinstance(frontmatter, dict):
            raise SkillConfigError(f"skill {path} frontmatter must be a mapping")

        name = frontmatter.get("name")
        description = frontmatter.get("description")
        if (
            not isinstance(name, str)
            or not 1 <= len(name) <= 64
            or _NAME_RE.fullmatch(name) is None
            or name != path.parent.name
        ):
            raise SkillConfigError(
                f"invalid skill name in {path}; name must match parent directory"
            )
        if not isinstance(description, str) or not 1 <= len(description) <= 1024:
            raise SkillConfigError(f"invalid skill description in {path}")

        metadata = frontmatter.get("metadata", {})
        if not isinstance(metadata, dict) or any(
            not isinstance(key, str) or not isinstance(value, str)
            for key, value in metadata.items()
        ):
            raise SkillConfigError(f"skill metadata in {path} must map strings to strings")

        schema_raw = metadata.get("zara-schema", "1")
        if schema_raw != "1":
            raise SkillConfigError(f"unsupported Zara skill schema {schema_raw!r} in {path}")
        priority = _parse_int(metadata.get("zara-priority", "0"), "priority", path, -10000, 10000)
        max_tokens = _parse_int(
            metadata.get("zara-max-tokens", "5000"),
            "max tokens",
            path,
            1,
            100000,
        )
        always_on = _parse_bool(metadata.get("zara-always-on", "false"), path)
        body = match.group(2).strip()
        if not body:
            raise SkillConfigError(f"skill {name!r} has an empty instruction body")
        body_tokens = int(self.token_counter(body))
        if body_tokens > max_tokens:
            raise SkillBudgetError(
                f"skill {name!r} body uses {body_tokens} tokens above declared budget {max_tokens}"
            )

        allowed_tools = frontmatter.get("allowed-tools")
        if allowed_tools is not None and not isinstance(allowed_tools, str):
            raise SkillConfigError(f"allowed-tools in {path} must be a string")
        license_name = frontmatter.get("license")
        compatibility = frontmatter.get("compatibility")
        if license_name is not None and not isinstance(license_name, str):
            raise SkillConfigError(f"license in {path} must be a string")
        if compatibility is not None and (
            not isinstance(compatibility, str) or not 1 <= len(compatibility) <= 500
        ):
            raise SkillConfigError(f"compatibility in {path} must be a bounded string")

        return SkillDefinition(
            name=name,
            description=description,
            body=body,
            path=path.resolve(),
            schema_version=1,
            domain=metadata.get("zara-domain", ""),
            language=metadata.get("zara-language", ""),
            selectors=_split(metadata.get("zara-selectors", "")),
            priority=priority,
            max_tokens=max_tokens,
            source_paths=_split(metadata.get("zara-paths", "")),
            dependencies=_split(metadata.get("zara-dependencies", "")),
            conflicts=_split(metadata.get("zara-conflicts", "")),
            always_on=always_on,
            allowed_tools=allowed_tools,
            license=license_name,
            compatibility=compatibility,
        )

    def _validate_graph(self, skills: dict[str, SkillDefinition]) -> None:
        for skill in skills.values():
            for dependency in skill.dependencies:
                if dependency not in skills:
                    raise SkillConfigError(
                        f"skill {skill.name!r} requires missing dependency {dependency!r}"
                    )

        visiting: set[str] = set()
        visited: set[str] = set()

        def visit(name: str) -> None:
            if name in visited:
                return
            if name in visiting:
                raise SkillConfigError(f"skill dependency cycle includes {name!r}")
            visiting.add(name)
            for dependency in skills[name].dependencies:
                visit(dependency)
            visiting.remove(name)
            visited.add(name)

        for name in sorted(skills):
            visit(name)

    def _add_dependencies(self, name: str, names: set[str]) -> None:
        for dependency in self._skills[name].dependencies:
            if dependency in names:
                continue
            names.add(dependency)
            self._add_dependencies(dependency, names)

    def _dependency_closure(self, name: str) -> tuple[str, ...]:
        result: list[str] = []
        seen: set[str] = set()

        def add(current: str) -> None:
            if current in seen:
                return
            seen.add(current)
            for dependency in self._skills[current].dependencies:
                add(dependency)
            result.append(current)

        add(name)
        return tuple(result)

    def _check_conflicts(self, names: set[str]) -> None:
        for name in sorted(names):
            skill = self._skills[name]
            for conflict in skill.conflicts:
                if conflict in names:
                    raise SkillConflictError(
                        f"selected skill {name!r} conflicts with {conflict!r}"
                    )


def _split(value: str) -> tuple[str, ...]:
    if not value.strip():
        return ()
    return tuple(part for part in _SPLIT_RE.split(value.strip()) if part)


def _parse_int(
    value: str,
    label: str,
    path: Path,
    minimum: int,
    maximum: int,
) -> int:
    try:
        parsed = int(value)
    except ValueError as error:
        raise SkillConfigError(f"invalid {label} in {path}") from error
    if not minimum <= parsed <= maximum:
        raise SkillConfigError(f"invalid {label} in {path}")
    return parsed


def _parse_bool(value: str, path: Path) -> bool:
    normalized = value.strip().lower()
    if normalized == "true":
        return True
    if normalized == "false":
        return False
    raise SkillConfigError(f"invalid zara-always-on value in {path}")


def _default_token_counter(text: str) -> int:
    return max(1, (len(text) + 3) // 4)
