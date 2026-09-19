#!/usr/bin/env python3
from __future__ import annotations

import argparse
import dataclasses
import json
import os
import re
import sys
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path
from typing import Iterable

TASK_RE = re.compile(r"^(\*{2})\s+(IDEA|TODO|NEXT|DOING|DONE|CANCELLED)\s+(.+?)\s*$")
PROPERTY_RE = re.compile(r"^:([A-Za-z0-9_@#%+-]+):\s*(.*?)\s*$")

OPEN_STATES = {"TODO", "NEXT", "DOING"}
CLOSED_STATES = {"DONE", "CANCELLED"}
SYNC_STATES = OPEN_STATES | CLOSED_STATES

RAGE_INITIAL_REQUIRED = (
    "RAGE_PARENT",
    "RAGE_PRIORITY",
    "RAGE_SCOPE",
    "RAGE_NON_GOALS",
    "RAGE_INVARIANTS",
    "RAGE_DEPENDS",
    "RAGE_BRANCH",
    "RAGE_RESEARCH",
    "RAGE_ACCEPTANCE",
    "RAGE_TDD",
    "RAGE_ARTIFACTS",
    "RAGE_GATE",
)

MANAGED_START = "<!-- zara-org-managed:start -->"
MANAGED_END = "<!-- zara-org-managed:end -->"


@dataclasses.dataclass(frozen=True)
class OrgTask:
    state: str
    title: str
    properties: dict[str, str]
    body: str
    line: int

    @property
    def task_id(self) -> str:
        return self.properties.get("ID", "").strip()

    @property
    def sync_mode(self) -> str:
        return self.properties.get("ISSUE_SYNC", "").strip().lower()

    @property
    def issue_number(self) -> int | None:
        raw = self.properties.get("ISSUE_NUMBER", "").strip().lstrip("#")
        if not raw:
            return None
        if not raw.isdigit():
            raise ValueError(f"line {self.line}: ISSUE_NUMBER must be numeric, got {raw!r}")
        return int(raw)


class ValidationError(RuntimeError):
    pass


def parse_org_text(text: str) -> list[OrgTask]:
    lines = text.splitlines()
    tasks: list[OrgTask] = []
    index = 0

    while index < len(lines):
        match = TASK_RE.match(lines[index])
        if not match:
            index += 1
            continue

        state = match.group(2)
        title = match.group(3).strip()
        start_line = index + 1
        index += 1

        properties: dict[str, str] = {}
        if index < len(lines) and lines[index].strip() == ":PROPERTIES:":
            index += 1
            while index < len(lines) and lines[index].strip() != ":END:":
                property_match = PROPERTY_RE.match(lines[index].strip())
                if property_match:
                    properties[property_match.group(1).upper()] = property_match.group(2).strip()
                elif lines[index].strip():
                    raise ValidationError(
                        f"line {index + 1}: invalid Org property line inside drawer"
                    )
                index += 1
            if index >= len(lines):
                raise ValidationError(f"line {start_line}: unterminated property drawer")
            index += 1

        body_lines: list[str] = []
        while index < len(lines):
            if TASK_RE.match(lines[index]):
                break
            body_lines.append(lines[index])
            index += 1

        tasks.append(
            OrgTask(
                state=state,
                title=title,
                properties=properties,
                body="\n".join(body_lines).strip(),
                line=start_line,
            )
        )

    return tasks


def parse_org(path: Path) -> list[OrgTask]:
    return parse_org_text(path.read_text(encoding="utf-8"))


def validate_tasks(tasks: Iterable[OrgTask]) -> None:
    errors: list[str] = []
    seen_ids: dict[str, int] = {}

    for task in tasks:
        task_id = task.task_id
        if task_id:
            previous = seen_ids.get(task_id)
            if previous is not None:
                errors.append(
                    f"line {task.line}: duplicate ID {task_id!r}; first seen on line {previous}"
                )
            else:
                seen_ids[task_id] = task.line

        if task.sync_mode != "auto":
            continue

        if task.state == "IDEA":
            continue

        if task.state not in SYNC_STATES:
            errors.append(f"line {task.line}: unsupported synced TODO state {task.state!r}")
            continue

        if not task_id:
            errors.append(f"line {task.line}: ISSUE_SYNC=auto requires stable :ID:")

        if task.properties.get("RAGE_SLICE", "").strip().lower() == "initial":
            missing = [
                key
                for key in RAGE_INITIAL_REQUIRED
                if not task.properties.get(key, "").strip()
            ]
            if missing:
                errors.append(
                    f"line {task.line}: initial RAGE slice missing properties: "
                    + ", ".join(missing)
                )

    if errors:
        raise ValidationError("\n".join(errors))


def marker(task_id: str) -> str:
    return f"<!-- zara-org-id: {task_id} -->"


def render_managed_block(task: OrgTask, source: str) -> str:
    fields = [
        ("Org source", source),
        ("Org state", task.state),
        ("RAGE slice", task.properties.get("RAGE_SLICE", "n/a")),
        ("Priority", task.properties.get("RAGE_PRIORITY", "n/a")),
        ("Parent", task.properties.get("RAGE_PARENT", "none")),
        ("Depends", task.properties.get("RAGE_DEPENDS", "none")),
        ("Branch hint", task.properties.get("RAGE_BRANCH", "none")),
    ]

    lines = [
        MANAGED_START,
        f"Managed from {source} entry {task.task_id}. Edit the Org entry for managed fields.",
        "",
        "## Org/RAGE metadata",
        "",
    ]
    lines.extend(f"- **{name}:** {value}" for name, value in fields)

    rage_details = [
        ("Scope", "RAGE_SCOPE"),
        ("Non-goals", "RAGE_NON_GOALS"),
        ("Invariants", "RAGE_INVARIANTS"),
        ("Research", "RAGE_RESEARCH"),
        ("Acceptance", "RAGE_ACCEPTANCE"),
        ("TDD", "RAGE_TDD"),
        ("Artifacts", "RAGE_ARTIFACTS"),
        ("Gate", "RAGE_GATE"),
    ]
    lines.extend(["", "## Initial slice definition", ""])
    lines.extend(
        f"- **{label}:** {task.properties.get(key, 'n/a')}"
        for label, key in rage_details
    )

    if task.body:
        lines.extend(["", "## Org specification", "", task.body])

    lines.append(MANAGED_END)
    return "\n".join(lines)


def merge_managed_body(existing_body: str, task: OrgTask, source: str) -> str:
    identity = marker(task.task_id)
    managed = render_managed_block(task, source)
    body = existing_body or ""

    start = body.find(MANAGED_START)
    end = body.find(MANAGED_END)
    if start >= 0 and end >= start:
        end += len(MANAGED_END)
        body = body[:start] + managed + body[end:]
    else:
        marker_pos = body.find(identity)
        if marker_pos >= 0:
            insert_at = marker_pos + len(identity)
            body = body[:insert_at] + "\n\n" + managed + body[insert_at:]
        else:
            body = identity + "\n\n" + managed + ("\n\n" + body if body.strip() else "")

    if identity not in body:
        body = identity + "\n\n" + body

    return body.strip() + "\n"


class GitHubClient:
    def __init__(self, token: str, api_url: str = "https://api.github.com") -> None:
        if not token:
            raise RuntimeError("GITHUB_TOKEN is required for --sync")
        self.token = token
        self.api_url = api_url.rstrip("/")

    def request(self, method: str, path: str, payload: dict | None = None) -> dict:
        url = f"{self.api_url}{path}"
        data = None
        headers = {
            "Accept": "application/vnd.github+json",
            "Authorization": f"Bearer {self.token}",
            "X-GitHub-Api-Version": "2022-11-28",
            "User-Agent": "zara-org-issue-sync",
        }
        if payload is not None:
            data = json.dumps(payload).encode("utf-8")
            headers["Content-Type"] = "application/json"

        request = urllib.request.Request(url, data=data, headers=headers, method=method)
        try:
            with urllib.request.urlopen(request, timeout=30) as response:
                raw = response.read()
        except urllib.error.HTTPError as exc:
            detail = exc.read().decode("utf-8", errors="replace")
            raise RuntimeError(f"GitHub API {method} {path} failed: {exc.code} {detail}") from exc

        if not raw:
            return {}
        return json.loads(raw.decode("utf-8"))

    def get_issue(self, repo: str, number: int) -> dict:
        return self.request("GET", f"/repos/{repo}/issues/{number}")

    def find_issue_by_marker(self, repo: str, task_id: str) -> dict | None:
        query = f'repo:{repo} in:body "{marker(task_id)}"'
        encoded = urllib.parse.urlencode({"q": query, "per_page": 10})
        result = self.request("GET", f"/search/issues?{encoded}")
        matches = [
            item
            for item in result.get("items", [])
            if marker(task_id) in (item.get("body") or "")
        ]
        if len(matches) > 1:
            numbers = ", ".join(str(item.get("number")) for item in matches)
            raise RuntimeError(f"duplicate managed issues for {task_id}: {numbers}")
        return matches[0] if matches else None

    def create_issue(self, repo: str, title: str, body: str, labels: list[str]) -> dict:
        payload: dict[str, object] = {"title": title, "body": body}
        if labels:
            payload["labels"] = labels
        return self.request("POST", f"/repos/{repo}/issues", payload)

    def update_issue(self, repo: str, number: int, payload: dict) -> dict:
        return self.request("PATCH", f"/repos/{repo}/issues/{number}", payload)


def issue_title(task: OrgTask) -> str:
    return task.properties.get("ISSUE_TITLE", "").strip() or task.title


def issue_labels(task: OrgTask) -> list[str]:
    raw = task.properties.get("ISSUE_LABELS", "")
    return [value.strip() for value in raw.split(",") if value.strip()]


def task_repo(task: OrgTask, default_repo: str) -> str:
    return task.properties.get("ISSUE_REPO", "").strip() or default_repo


def sync_task(
    task: OrgTask,
    client: GitHubClient,
    default_repo: str,
    source: str,
    dry_run: bool = False,
) -> str:
    if task.sync_mode != "auto" or task.state == "IDEA":
        return "ignored"

    repo = task_repo(task, default_repo)
    if not repo:
        raise RuntimeError(f"line {task.line}: ISSUE_REPO or GITHUB_REPOSITORY is required")

    existing = None
    number = task.issue_number
    if number is not None:
        existing = client.get_issue(repo, number)
        if marker(task.task_id) not in (existing.get("body") or ""):
            raise RuntimeError(
                f"{repo}#{number} does not contain ownership marker for {task.task_id}"
            )
    else:
        existing = client.find_issue_by_marker(repo, task.task_id)
        if existing is not None:
            number = int(existing["number"])

    desired_title = issue_title(task)
    existing_body = (existing or {}).get("body") or ""
    desired_body = merge_managed_body(existing_body, task, source)

    if task.state in OPEN_STATES:
        if existing is None:
            if dry_run:
                return f"would-create {repo}: {desired_title}"
            created = client.create_issue(repo, desired_title, desired_body, issue_labels(task))
            return f"created {repo}#{created['number']}"

        payload: dict[str, object] = {}
        if existing.get("title") != desired_title:
            payload["title"] = desired_title
        if existing_body != desired_body:
            payload["body"] = desired_body
        if existing.get("state") != "open":
            payload["state"] = "open"

        if not payload:
            return f"unchanged {repo}#{number}"
        if dry_run:
            return f"would-update {repo}#{number}"
        client.update_issue(repo, number, payload)
        return f"updated {repo}#{number}"

    if existing is None:
        return "ignored-closed-without-issue"

    if existing.get("state") == "closed" and existing_body == desired_body:
        return f"unchanged {repo}#{number}"

    payload = {}
    if existing_body != desired_body:
        payload["body"] = desired_body
    if existing.get("title") != desired_title:
        payload["title"] = desired_title
    if existing.get("state") != "closed":
        payload["state"] = "closed"
        payload["state_reason"] = "completed" if task.state == "DONE" else "not_planned"

    if not payload:
        return f"unchanged {repo}#{number}"
    if dry_run:
        return f"would-close {repo}#{number}"
    client.update_issue(repo, number, payload)
    return f"closed {repo}#{number}"


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Validate/sync Zara Org TODOs to GitHub Issues")
    parser.add_argument("--file", default="ideas.org", help="Org queue file")
    parser.add_argument("--repo", default=os.getenv("GITHUB_REPOSITORY", ""))
    parser.add_argument("--source", default=None, help="Source label shown in managed issue body")
    parser.add_argument("--dry-run", action="store_true")
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--check", action="store_true", help="Validate only")
    mode.add_argument("--sync", action="store_true", help="Validate and sync GitHub issues")
    return parser


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    path = Path(args.file)
    tasks = parse_org(path)

    try:
        validate_tasks(tasks)
    except ValidationError as exc:
        print(str(exc), file=sys.stderr)
        return 2

    if args.check:
        managed = sum(
            1 for task in tasks if task.sync_mode == "auto" and task.state in SYNC_STATES
        )
        print(f"validated {len(tasks)} Org task(s); {managed} issue-managed task(s)")
        return 0

    client = GitHubClient(os.getenv("GITHUB_TOKEN", ""))
    source = args.source or path.as_posix()
    for task in tasks:
        result = sync_task(
            task,
            client=client,
            default_repo=args.repo,
            source=source,
            dry_run=args.dry_run,
        )
        if result != "ignored":
            print(f"{task.task_id or '<no-id>'}: {result}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
