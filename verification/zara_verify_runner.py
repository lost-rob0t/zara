"""Bounded host collector for ZARA-VERIFY/1; policy decisions live in Prolog.

Local receipts are integrity/freshness evidence, not a same-UID security boundary.
Never expose a caller-supplied receipt as trusted verifier provenance (#662).
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import selectors
import shutil
import signal
import stat
import subprocess
import sys
import tempfile
import threading
import time
import uuid
import xml.etree.ElementTree as ET
from typing import Any

PROTOCOL = 'ZARA-VERIFY/1'
SOURCE_BASE_SENTINEL = '@SOURCE_BASE@'
MAX_FILE_BYTES = 16 * 1024 * 1024
MAX_SOURCE_BYTES = 256 * 1024 * 1024
MAX_FILES = 20000
MAX_PATHS = 4096
MAX_JSON_BYTES = 2 * 1024 * 1024
MAX_NAMESPACE_EVENTS = 100000
MAX_NAMESPACE_PATH_BYTES = 8192
PROTECTED = (
    'verification/zara_verify_runner.py', 'verification/zara_verify.pl',
    'verification/zara_verifier_expert.py', 'contracts/zara-verify-v1/spec.json',
    '.opencode/plugins/zara-verify.js', '.opencode/lib/zara-verify.mjs',
)


class VerificationError(ValueError):
    """Verification cannot establish its required evidence."""


def canonical_digest(value: Any) -> str:
    encoded = json.dumps(value, sort_keys=True, separators=(',', ':'),
                         ensure_ascii=True, allow_nan=False).encode()
    return hashlib.sha256(encoded).hexdigest()


def git_bytes(root: Path, *args: str) -> bytes:
    try:
        result = subprocess.run(['git', '--no-optional-locks', '-c', 'core.fsmonitor=false', '-c', 'core.hooksPath=/dev/null', '-c', 'core.excludesFile=/dev/null', '-C', str(root), *args],
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                timeout=15, check=True)
    except (OSError, subprocess.SubprocessError) as error:
        raise VerificationError('git_observation_failed') from error
    if len(result.stdout) > MAX_JSON_BYTES:
        raise VerificationError('git_output_limit')
    return result.stdout


def policy_digest(policy_root: Path) -> str:
    entries = []
    for name in PROTECTED:
        path = policy_root / name
        if path.is_symlink() or not path.is_file():
            raise VerificationError('missing_or_symlinked_verifier: ' + name)
        with path.open('rb') as stream:
            data = stream.read(MAX_FILE_BYTES + 1)
        if len(data) > MAX_FILE_BYTES:
            raise VerificationError('verifier_size_limit')
        entries.append((name, hashlib.sha256(data).hexdigest()))
    return canonical_digest(entries)


def safe_path(raw: bytes) -> str:
    try:
        value = raw.decode('utf-8', errors='strict')
    except UnicodeDecodeError as error:
        raise VerificationError('non_utf8_path') from error
    if (not value or len(value) > 4096 or value.startswith('/') or
            '..' in value.split('/') or any(ord(char) < 32 for char in value)):
        raise VerificationError('unsafe_source_path')
    return value


def repository_exclude_path(root: Path) -> Path:
    root = Path(root).resolve()
    raw = git_bytes(root, 'rev-parse', '--git-path', 'info/exclude').decode().strip()
    path = Path(raw)
    if not path.is_absolute():
        path = root / path
    return path.resolve(strict=False)


def source_authority_paths(root: Path, policy_root: Path) -> list[Path]:
    root = Path(root).resolve()
    policy_root = Path(policy_root).resolve()
    tracked = git_bytes(root, 'ls-files', '--cached', '-z')
    untracked = git_bytes(root, 'ls-files', '--others', '--exclude-standard', '-z')
    names = sorted({safe_path(raw) for raw in (tracked + untracked).split(b'\0') if raw})
    if len(names) > MAX_FILES:
        raise VerificationError('source_file_count_limit')
    targets = {root / name for name in names}
    targets.update(policy_root / name for name in PROTECTED)
    targets.add(repository_exclude_path(root))
    return sorted(targets, key=lambda path: os.fsencode(str(path)))


def mutation_epoch_entry(path: Path) -> tuple[Any, ...]:
    try:
        info = path.lstat()
        return ('present', info.st_dev, info.st_ino, info.st_mode,
                info.st_size, info.st_ctime_ns)
    except FileNotFoundError:
        return ('missing',)


def capture_mutation_epoch(root: Path, policy_root: Path) -> dict[str, tuple[Any, ...]]:
    return {str(path): mutation_epoch_entry(path)
            for path in source_authority_paths(root, policy_root)}


def assert_mutation_epoch(epoch: dict[str, tuple[Any, ...]]) -> None:
    for raw_path, expected in epoch.items():
        if mutation_epoch_entry(Path(raw_path)) != expected:
            raise VerificationError('source_mutated_during_verification')


def ignored_directory_prefixes(root: Path) -> tuple[str, ...]:
    raw = git_bytes(root, 'ls-files', '--others', '--ignored',
                    '--exclude-standard', '--directory', '-z')
    return tuple(sorted({
        safe_path(item[:-1]) for item in raw.split(b'\0')
        if item and item.endswith(b'/')
    }))


def source_namespace_directories(root: Path) -> list[Path]:
    root = Path(root).resolve()
    ignored = ignored_directory_prefixes(root)
    directories = []
    for current, children, _files in os.walk(root, topdown=True, followlinks=False):
        current_path = Path(current)
        kept = []
        for name in children:
            child = current_path / name
            if child.is_symlink():
                continue
            relative = child.relative_to(root).as_posix()
            if relative == '.git':
                continue
            safe_path(relative.encode())
            if any(relative == prefix or relative.startswith(prefix + '/') for prefix in ignored):
                continue
            kept.append(name)
        children[:] = kept
        directories.append(current_path)
        if len(directories) > MAX_FILES * 2:
            raise VerificationError('namespace_watch_path_limit')
    return directories


def policy_namespace_directories(policy_root: Path, source_root: Path) -> list[Path]:
    policy_root = Path(policy_root).resolve()
    if policy_root == Path(source_root).resolve():
        return []
    directories = []
    for current, children, _files in os.walk(policy_root, topdown=True, followlinks=False):
        current_path = Path(current)
        children[:] = [name for name in children if not (current_path / name).is_symlink()]
        directories.append(current_path)
        if len(directories) > MAX_FILES:
            raise VerificationError('namespace_watch_path_limit')
    return directories


def git_ignored(root: Path, relative: str) -> bool:
    env = os.environ.copy()
    env['GIT_CONFIG_GLOBAL'] = os.devnull
    env['GIT_CONFIG_SYSTEM'] = os.devnull
    try:
        result = subprocess.run(
            ['git', '--no-optional-locks', '-c', 'core.fsmonitor=false',
             '-c', 'core.hooksPath=/dev/null', '-c', 'core.excludesFile=/dev/null',
             '-C', str(root), 'check-ignore', '-q', '--no-index', '--', relative],
            stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, timeout=5, env=env)
    except (OSError, subprocess.SubprocessError) as error:
        raise VerificationError('git_ignore_observation_failed') from error
    if result.returncode == 0:
        return True
    if result.returncode == 1:
        return False
    raise VerificationError('git_ignore_observation_failed')


def namespace_relative(raw_path: str, boundary: Path) -> str | None:
    boundary_text = os.path.abspath(os.fspath(boundary))
    path_text = os.path.abspath(raw_path)
    try:
        common = os.path.commonpath([boundary_text, path_text])
    except ValueError as error:
        raise VerificationError('namespace_path_escape') from error
    if common != boundary_text or path_text == boundary_text:
        return None
    relative = os.path.relpath(path_text, boundary_text).replace(os.sep, '/')
    return safe_path(relative.encode('utf-8', errors='strict'))


class NamespaceMutationWatch:
    def __init__(self, root: Path, policy_root: Path):
        self.root = Path(root).resolve()
        self.policy_root = Path(policy_root).resolve()
        self.process = None
        self.reader = None
        self.condition = threading.Condition()
        self.events = []
        self.cursor = 0
        self.overflow = False
        self.temp = None
        self.barrier_root = None

    def start(self) -> None:
        executable = shutil.which('inotifywait')
        if executable is None:
            raise VerificationError('namespace_watcher_unavailable')
        self.temp = tempfile.TemporaryDirectory(prefix='zara-verify-namespace-')
        temp_root = Path(self.temp.name)
        self.barrier_root = temp_root / 'barrier'
        self.barrier_root.mkdir()
        watch_paths = set(source_namespace_directories(self.root))
        watch_paths.update(policy_namespace_directories(self.policy_root, self.root))
        watch_paths.add(self.barrier_root)
        watch_list = temp_root / 'watch-list'
        encoded = []
        for path in sorted(watch_paths, key=lambda item: os.fsencode(str(item))):
            raw = os.fsencode(str(path))
            if any(byte < 32 for byte in raw):
                raise VerificationError('unsafe_namespace_watch_path')
            encoded.append(raw)
        watch_list.write_bytes(b'\n'.join(encoded) + b'\n')
        self.process = subprocess.Popen(
            [executable, '--monitor', '--no-dereference',
             '--event', 'create', '--event', 'delete',
             '--event', 'moved_from', '--event', 'moved_to',
             '--format', '%w%f%0', '--no-newline',
             '--fromfile', str(watch_list)],
            stdin=subprocess.DEVNULL, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            start_new_session=True)
        self._wait_ready()
        self.reader = threading.Thread(target=self._read_events, daemon=True)
        self.reader.start()

    def _wait_ready(self) -> None:
        deadline = time.monotonic() + 5
        buffered = b''
        with selectors.DefaultSelector() as selector:
            selector.register(self.process.stderr, selectors.EVENT_READ)
            while b'Watches established.\n' not in buffered:
                if self.process.poll() is not None:
                    raise VerificationError('namespace_watcher_start_failed')
                remaining = deadline - time.monotonic()
                if remaining <= 0:
                    raise VerificationError('namespace_watcher_start_timeout')
                if not selector.select(min(0.1, remaining)):
                    continue
                chunk = os.read(self.process.stderr.fileno(), 4096)
                if not chunk:
                    raise VerificationError('namespace_watcher_start_failed')
                buffered += chunk

    def _read_events(self) -> None:
        buffered = b''
        try:
            while True:
                chunk = os.read(self.process.stdout.fileno(), 65536)
                if not chunk:
                    break
                buffered += chunk
                while b'\0' in buffered:
                    raw, buffered = buffered.split(b'\0', 1)
                    with self.condition:
                        if len(raw) > MAX_NAMESPACE_PATH_BYTES or len(self.events) >= MAX_NAMESPACE_EVENTS:
                            self.overflow = True
                        else:
                            self.events.append(raw)
                        self.condition.notify_all()
        except OSError:
            pass

    def _sync(self) -> None:
        if self.process is None or self.barrier_root is None:
            raise VerificationError('namespace_watcher_not_started')
        marker = self.barrier_root / ('barrier-' + uuid.uuid4().hex)
        marker.write_bytes(b'')
        expected = os.fsencode(str(marker))
        deadline = time.monotonic() + 5
        with self.condition:
            while expected not in self.events:
                if self.overflow:
                    raise VerificationError('namespace_event_limit')
                if self.process.poll() is not None:
                    raise VerificationError('namespace_watcher_failed')
                remaining = deadline - time.monotonic()
                if remaining <= 0:
                    raise VerificationError('namespace_watcher_sync_timeout')
                self.condition.wait(timeout=min(0.05, remaining))
        marker.unlink(missing_ok=True)

    def assert_clean(self) -> None:
        self._sync()
        with self.condition:
            if self.overflow:
                raise VerificationError('namespace_event_limit')
            pending = self.events[self.cursor:]
            self.cursor = len(self.events)
        barrier_root = os.path.abspath(os.fspath(self.barrier_root))
        protected_dirs = {
            os.path.abspath(os.fspath((self.policy_root / name).parent))
            for name in PROTECTED
        }
        for raw in pending:
            try:
                raw_path = raw.decode('utf-8', errors='strict')
            except UnicodeDecodeError as error:
                raise VerificationError('non_utf8_namespace_path') from error
            absolute = os.path.abspath(raw_path)
            if os.path.commonpath([barrier_root, absolute]) == barrier_root:
                continue
            policy_relative = namespace_relative(absolute, self.policy_root)
            if policy_relative is not None and any(
                    os.path.commonpath([directory, absolute]) == directory
                    for directory in protected_dirs):
                raise VerificationError('source_mutated_during_verification')
            source_relative = namespace_relative(absolute, self.root)
            if source_relative is not None:
                if git_ignored(self.root, source_relative):
                    continue
                raise VerificationError('source_mutated_during_verification')
            if policy_relative is not None:
                raise VerificationError('source_mutated_during_verification')
            raise VerificationError('namespace_watcher_foreign_event')

    def close(self) -> None:
        if self.process is not None:
            if self.process.poll() is None:
                try:
                    os.killpg(self.process.pid, signal.SIGTERM)
                    self.process.wait(timeout=2)
                except (ProcessLookupError, subprocess.TimeoutExpired):
                    if self.process.poll() is None:
                        terminate_group(self.process)
            if self.process.stdout is not None:
                self.process.stdout.close()
            if self.process.stderr is not None:
                self.process.stderr.close()
        if self.reader is not None:
            self.reader.join(timeout=2)
        if self.temp is not None:
            self.temp.cleanup()


def collect_snapshot(root: Path, base_ref: str, policy_root: Path) -> dict[str, Any]:
    root = Path(root).resolve()
    if not base_ref or base_ref.startswith('-') or any(ord(c) < 33 for c in base_ref):
        raise VerificationError('invalid_base_ref')
    observed_root = Path(git_bytes(root, 'rev-parse', '--show-toplevel').decode().strip()).resolve()
    if observed_root != root:
        raise VerificationError('workspace_must_be_repository_root')
    head = git_bytes(root, 'rev-parse', '--verify', 'HEAD^{commit}').decode().strip()
    base = git_bytes(root, 'rev-parse', '--verify', '--end-of-options',
                     base_ref + '^{commit}').decode().strip()
    merge_base = git_bytes(root, 'merge-base', head, base).decode().strip()
    stage = git_bytes(root, 'ls-files', '--stage', '-z')
    if any(record.startswith(b'160000 ') for record in stage.split(b'\0')):
        raise VerificationError('submodule_source_requires_dedicated_collector')
    tracked = git_bytes(root, 'ls-files', '--cached', '-z')
    untracked = git_bytes(root, 'ls-files', '--others', '--exclude-standard', '-z')
    paths = sorted({safe_path(raw) for raw in (tracked + untracked).split(b'\0') if raw})
    if len(paths) > MAX_FILES:
        raise VerificationError('source_file_count_limit')
    source = hashlib.sha256()
    size = 0
    for name in paths:
        path = root / name
        try:
            info = path.lstat()
            if stat.S_ISLNK(info.st_mode):
                data, kind = os.readlink(path).encode(), 'symlink'
            elif stat.S_ISREG(info.st_mode):
                if info.st_size > MAX_FILE_BYTES:
                    raise VerificationError('source_file_size_limit: ' + name)
                fd = os.open(path, os.O_RDONLY | getattr(os, 'O_NOFOLLOW', 0))
                try:
                    opened = os.fstat(fd)
                    observed = (info.st_dev, info.st_ino, info.st_mode, info.st_size,
                                info.st_mtime_ns, info.st_ctime_ns)
                    opened_id = (opened.st_dev, opened.st_ino, opened.st_mode, opened.st_size,
                                 opened.st_mtime_ns, opened.st_ctime_ns)
                    if opened_id != observed:
                        raise VerificationError('source_changed_during_read')
                    with os.fdopen(fd, 'rb', closefd=False) as stream:
                        data = stream.read(MAX_FILE_BYTES + 1)
                    after = os.fstat(fd)
                    after_id = (after.st_dev, after.st_ino, after.st_mode, after.st_size,
                                after.st_mtime_ns, after.st_ctime_ns)
                    current = path.lstat()
                    current_id = (current.st_dev, current.st_ino, current.st_mode, current.st_size,
                                  current.st_mtime_ns, current.st_ctime_ns)
                    if after_id != opened_id or current_id != opened_id:
                        raise VerificationError('source_changed_during_read')
                finally:
                    os.close(fd)
                if len(data) > MAX_FILE_BYTES:
                    raise VerificationError('source_file_size_limit: ' + name)
                kind = 'executable' if info.st_mode & 0o111 else 'file'
            else:
                raise VerificationError('unsupported_source_type: ' + name)
        except FileNotFoundError:
            data, kind = b'', 'deleted'
        size += len(data)
        if size > MAX_SOURCE_BYTES:
            raise VerificationError('source_total_size_limit')
        source.update(json.dumps([name, kind, hashlib.sha256(data).hexdigest()]).encode())
        source.update(b'\0')
    exclude_path = repository_exclude_path(root)
    try:
        exclude_data = retained_file_bytes(exclude_path, MAX_FILE_BYTES)
        exclude_state = 'present'
    except FileNotFoundError:
        exclude_data = b''
        exclude_state = 'missing'
    source.update(json.dumps([
        'repository_exclude', exclude_state, hashlib.sha256(exclude_data).hexdigest()
    ]).encode())
    source.update(b'\0')
    changed = git_bytes(root, 'diff', '--no-ext-diff', '--name-only', '--no-renames',
                        '-z', merge_base, '--') + untracked
    changed_paths = sorted({safe_path(raw) for raw in changed.split(b'\0') if raw})
    if len(changed_paths) > MAX_PATHS:
        raise VerificationError('changed_path_limit')
    status = git_bytes(root, 'status', '--porcelain=v1', '-z', '--untracked-files=all')
    source.update(stage)
    source.update(status)
    return {'workspace': str(root), 'head': head, 'base': base, 'merge_base': merge_base,
            'worktree': source.hexdigest(), 'policy': policy_digest(policy_root),
            'changed_paths': changed_paths, 'clean': not bool(status)}


def current_report(report: Any, snapshot: dict[str, Any], now_ms: int | None = None) -> bool:
    """Freshness check ONLY for a report already observed from the trusted host."""
    if not isinstance(report, dict):
        return False
    now = int(time.time() * 1000) if now_ms is None else now_ms
    created, ttl = report.get('created_ms'), report.get('ttl_ms')
    return (
        report.get('protocol') == PROTOCOL and report.get('scope') == 'local'
        and report.get('verdict') == 'verified' and report.get('source') == snapshot
        and report.get('merge_authorized') is False
        and type(report.get('model_calls')) is int and report['model_calls'] == 0
        and type(report.get('provider_calls')) is int and report['provider_calls'] == 0
        and type(created) is int and type(ttl) is int and 0 < ttl <= 600000
        and created <= now < created + ttl and report.get('reasons') == []
    )


def load_spec(policy_root: Path) -> dict[str, Any]:
    with (policy_root / 'contracts/zara-verify-v1/spec.json').open('rb') as stream:
        raw = stream.read(MAX_JSON_BYTES + 1)
    if len(raw) > MAX_JSON_BYTES:
        raise VerificationError('spec_size_limit')
    spec = json.loads(raw)
    if spec.get('protocol') != PROTOCOL or not isinstance(spec.get('gates'), dict):
        raise VerificationError('incompatible_spec')
    if not 1 <= len(spec['gates']) <= 64:
        raise VerificationError('gate_count_limit')
    for gate_id, gate in spec['gates'].items():
        if not re.fullmatch(r'[a-z][a-z0-9_]{0,63}', gate_id):
            raise VerificationError('invalid_gate_id')
        if not isinstance(gate, dict):
            raise VerificationError('invalid_gate')
        if set(gate) - {'argv', 'timeout_seconds', 'requires', 'collector', 'junit'}:
            raise VerificationError('unknown_gate_field')
        timeout = gate.get('timeout_seconds')
        if type(timeout) is not int or not 1 <= timeout <= 3600:
            raise VerificationError('invalid_gate_timeout')
        command = gate.get('argv', [])
        if not isinstance(command, list) or len(command) > 32:
            raise VerificationError('invalid_gate_command')
        if any(not isinstance(x, str) or not x or len(x) > 4096 or '\0' in x for x in command):
            raise VerificationError('invalid_gate_argument')
        if any(SOURCE_BASE_SENTINEL in x and x != SOURCE_BASE_SENTINEL for x in command):
            raise VerificationError('invalid_gate_base_placeholder')
        if gate.get('collector') not in {'local', 'external'}:
            raise VerificationError('invalid_collector')
        if gate['collector'] == 'local' and not command:
            raise VerificationError('missing_gate_command')
        if gate['collector'] == 'external' and command:
            raise VerificationError('external_gate_cannot_execute')
        dependencies = gate.get('requires', [])
        if (not isinstance(dependencies, list) or len(dependencies) > 64
                or any(not isinstance(item, str) for item in dependencies)
                or len(set(dependencies)) != len(dependencies)):
            raise VerificationError('invalid_gate_dependencies')
        if 'junit' in gate:
            junit = gate['junit']
            if not isinstance(junit, str):
                raise VerificationError('invalid_junit_path')
            safe_path(junit.encode())
        if any(item not in spec['gates'] for item in dependencies):
            raise VerificationError('unknown_gate_dependency')
    return spec


def materialize_gate_argv(argv: list[str], source_base: str) -> list[str]:
    if not re.fullmatch(r'(?:[0-9a-f]{40}|[0-9a-f]{64})', source_base):
        raise VerificationError('invalid_source_base')
    materialized = []
    for argument in argv:
        if SOURCE_BASE_SENTINEL in argument:
            if argument != SOURCE_BASE_SENTINEL:
                raise VerificationError('invalid_gate_base_placeholder')
            materialized.append(source_base)
        else:
            materialized.append(argument)
    return materialized


def gate_environment(directory: Path) -> dict[str, str]:
    directory.mkdir(parents=True, exist_ok=True)
    env = {key: os.environ[key] for key in ('PATH', 'NIX_SSL_CERT_FILE', 'SSL_CERT_FILE')
           if key in os.environ}
    for key, suffix in [('HOME', 'home'), ('XDG_CONFIG_HOME', 'config'),
                        ('XDG_DATA_HOME', 'data'), ('XDG_RUNTIME_DIR', 'runtime')]:
        path = directory / suffix
        path.mkdir(exist_ok=True, mode=0o700)
        env[key] = str(path)
    env.update({'LANG': 'C.UTF-8', 'LC_ALL': 'C.UTF-8', 'CI': 'true',
                'QT_QPA_PLATFORM': 'offscreen', 'ARTIFACT_DIR': str(directory),
                'PYTHONDONTWRITEBYTECODE': '1'})
    return env


def terminate_group(process: subprocess.Popen) -> None:
    try:
        os.killpg(process.pid, signal.SIGKILL)
    except ProcessLookupError:
        pass
    process.wait(timeout=5)


def retained_file_bytes(path: Path, max_bytes: int) -> bytes:
    if max_bytes <= 0:
        raise VerificationError('invalid_artifact_limit')
    observed = path.lstat()
    if stat.S_ISLNK(observed.st_mode) or not stat.S_ISREG(observed.st_mode):
        raise VerificationError('unsafe_artifact_type')
    fd = os.open(path, os.O_RDONLY | getattr(os, 'O_NOFOLLOW', 0))
    try:
        opened = os.fstat(fd)
        if (not stat.S_ISREG(opened.st_mode)
                or (opened.st_dev, opened.st_ino) != (observed.st_dev, observed.st_ino)):
            raise VerificationError('unsafe_artifact_type')
        chunks = []
        remaining = max_bytes + 1
        while remaining:
            chunk = os.read(fd, min(65536, remaining))
            if not chunk:
                break
            chunks.append(chunk)
            remaining -= len(chunk)
        data = b''.join(chunks)
        if len(data) > max_bytes:
            raise VerificationError('artifact_size_limit')
        return data
    finally:
        os.close(fd)


def execute_gate(gate_id: str, argv: list[str], root: Path, directory: Path,
                 timeout: float, max_output: int = MAX_JSON_BYTES) -> dict[str, Any]:
    if not argv or timeout <= 0 or max_output <= 0:
        raise VerificationError('invalid_execution_bounds')
    directory.mkdir(parents=True, exist_ok=True, mode=0o700)
    log_path = directory / 'output.log'
    started = time.monotonic()
    state, reason, code = 'error', 'not_started', None
    process = None
    written = 0
    digest = hashlib.sha256()
    with log_path.open('wb', buffering=0) as output:
        try:
            process = subprocess.Popen(argv, cwd=root, env=gate_environment(directory),
                                       stdin=subprocess.DEVNULL, stdout=subprocess.PIPE,
                                       stderr=subprocess.STDOUT, start_new_session=True)
            with selectors.DefaultSelector() as selector:
                selector.register(process.stdout, selectors.EVENT_READ)
                eof = False
                while not eof:
                    remaining = timeout - (time.monotonic() - started)
                    if remaining <= 0:
                        reason = 'timeout'
                        break
                    events = selector.select(min(remaining, 0.05))
                    for key, _ in events:
                        chunk = os.read(key.fileobj.fileno(), 65536)
                        if not chunk:
                            eof = True
                            break
                        permitted = chunk[:max_output - written]
                        output.write(permitted)
                        digest.update(permitted)
                        written += len(permitted)
                        if len(permitted) != len(chunk):
                            reason, eof = 'output_limit', True
                            break
                if reason not in {'timeout', 'output_limit'}:
                    code = process.wait(timeout=max(0.001, timeout - (time.monotonic() - started)))
                    state, reason = ('passed', 'exit_zero') if code == 0 else ('failed', 'nonzero_exit')
        except FileNotFoundError:
            state, reason = 'missing', 'executable_unavailable'
        except subprocess.TimeoutExpired:
            state, reason = 'error', 'timeout'
        except KeyboardInterrupt:
            state, reason = 'cancelled', 'interrupted'
        except OSError:
            state, reason = 'error', 'execution_error'
        finally:
            if process is not None:
                terminate_group(process)
                if process.stdout is not None:
                    process.stdout.close()
                code = process.returncode
    retained_digest = ''
    retained_bytes = 0
    try:
        retained = retained_file_bytes(log_path, max_output)
        retained_digest = hashlib.sha256(retained).hexdigest()
        retained_bytes = len(retained)
        if retained_bytes != written or retained_digest != digest.hexdigest():
            state, reason = 'error', 'artifact_tampered'
    except (OSError, VerificationError):
        state, reason = 'error', 'artifact_tampered'
    return {'gate': gate_id, 'state': state, 'reason': reason, 'exit_code': code,
            'duration_ms': int((time.monotonic() - started) * 1000),
            'artifact': str(log_path), 'artifact_sha256': retained_digest, 'bytes': retained_bytes}


def junit_summary_failed(root: ET.Element) -> bool:
    for element in root.iter():
        if element.tag not in {'testsuite', 'testsuites'}:
            continue
        for field in ('failures', 'errors', 'skipped'):
            raw = element.get(field)
            if raw is None:
                continue
            if re.fullmatch(r'(?:0|[1-9][0-9]*)', raw) is None:
                raise VerificationError('invalid_junit_summary')
            if raw != '0':
                return True
    return False


def parse_junit(path: Path, include_digest: bool = False) -> dict[str, Any]:
    try:
        data = retained_file_bytes(path, MAX_JSON_BYTES)
        if b'<!DOCTYPE' in data.upper() or b'<!ENTITY' in data.upper():
            raise VerificationError('unsafe_junit')
        root = ET.fromstring(data)
        if root.tag not in {'testsuite', 'testsuites'}:
            raise VerificationError('invalid_junit_root')
        if any(not isinstance(element.tag, str) or element.tag.startswith('{')
               for element in root.iter()):
            raise VerificationError('unsupported_junit_namespace')
        summary_failed = junit_summary_failed(root)
        cases = list(root.iter('testcase'))
        failures = sum(case.find('failure') is not None or case.find('error') is not None for case in cases)
        skipped = sum(case.find('skipped') is not None for case in cases)
        state = ('passed' if len(cases) > skipped and failures == 0 and not summary_failed
                 else 'failed')
        result = {'state': state, 'tests': len(cases), 'failures': failures, 'skipped': skipped}
        if include_digest:
            result['sha256'] = hashlib.sha256(data).hexdigest()
        return result
    except (OSError, ET.ParseError, VerificationError):
        return {'state': 'missing', 'tests': 0, 'failures': 0, 'skipped': 0}


def invoke_policy(request: dict[str, Any], policy_root: Path) -> dict[str, Any]:
    if shutil.which('swipl') is None:
        raise VerificationError('prolog_unavailable')
    encoded = json.dumps(request, allow_nan=False).encode()
    if len(encoded) > MAX_JSON_BYTES:
        raise VerificationError('policy_input_limit')
    try:
        result = subprocess.run(['swipl', '-q', '-f', 'none', '-s',
                                 str(policy_root / 'verification/zara_verify.pl'), '-g',
                                 'zara_verify:main', '-t', 'halt(2)'],
                                input=encoded, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                timeout=10, check=True)
        if len(result.stdout) > MAX_JSON_BYTES:
            raise VerificationError('policy_output_limit')
        answer = json.loads(result.stdout)
        if not isinstance(answer, dict) or answer.get('protocol') != PROTOCOL:
            raise VerificationError('invalid_policy_response')
        return answer
    except (OSError, subprocess.SubprocessError, ValueError) as error:
        raise VerificationError('policy_failed') from error


def plan_verification(root: Path, base_ref: str, policy_root: Path) -> dict[str, Any]:
    source = collect_snapshot(root, base_ref, policy_root)
    answer = invoke_policy({'operation': 'plan', 'source': source}, policy_root)
    return {**answer, 'source': source}


def run_verification(root: Path, base_ref: str, policy_root: Path,
                     session_id: str) -> dict[str, Any]:
    run_id = uuid.uuid4().hex
    created = int(time.time() * 1000)
    report: dict[str, Any] = {
        'protocol': PROTOCOL, 'scope': 'local', 'verdict': 'blocked', 'run_id': run_id,
        'session_id': session_id, 'created_ms': created, 'ttl_ms': 600000,
        'model_calls': 0, 'provider_calls': 0, 'merge_authorized': False,
        'reasons': [], 'evidence': [],
    }
    namespace_watch = None
    try:
        if shutil.which('swipl') is None:
            raise VerificationError('prolog_unavailable')
        epoch = capture_mutation_epoch(root, policy_root)
        namespace_watch = NamespaceMutationWatch(root, policy_root)
        namespace_watch.start()
        source = collect_snapshot(root, base_ref, policy_root)
        namespace_watch.assert_clean()
        assert_mutation_epoch(epoch)
        report['source'] = source
        spec = load_spec(policy_root)
        plan = invoke_policy({'operation': 'plan', 'source': source}, policy_root)
        required = plan.get('required')
        if not isinstance(required, list) or not required or len(set(required)) != len(required):
            raise VerificationError('invalid_plan')
        if any(gate not in spec['gates'] for gate in required):
            raise VerificationError('unknown_planned_gate')
        git_dir = Path(git_bytes(root, 'rev-parse', '--absolute-git-dir').decode().strip())
        evidence_dir = git_dir / 'zara-verify' / run_id
        evidence_dir.mkdir(parents=True, mode=0o700)
        pending, results = list(required), {}
        deadline = time.monotonic() + 7200
        while pending:
            progress = False
            for gate_id in pending[:]:
                gate = spec['gates'][gate_id]
                dependencies = gate.get('requires', [])
                if any(dep not in required for dep in dependencies):
                    raise VerificationError('incomplete_dependency_plan')
                if any(dep not in results for dep in dependencies):
                    continue
                pending.remove(gate_id)
                progress = True
                failure = next((dep for dep in dependencies if results[dep]['state'] != 'passed'), None)
                if failure or time.monotonic() >= deadline:
                    item = {'gate': gate_id, 'state': 'blocked', 'reason': 'dependency_or_deadline',
                            'exit_code': None, 'artifact_sha256': '', 'bytes': 0}
                elif gate['collector'] == 'external':
                    item = {'gate': gate_id, 'state': 'missing', 'reason': 'independent_evidence_required',
                            'exit_code': None, 'artifact_sha256': '', 'bytes': 0}
                else:
                    timeout = min(gate['timeout_seconds'], deadline - time.monotonic())
                    argv = materialize_gate_argv(gate['argv'], source['base'])
                    item = execute_gate(gate_id, argv, root, evidence_dir / gate_id, timeout)
                    if gate.get('junit') and item['state'] == 'passed':
                        junit_path = evidence_dir / gate_id / gate['junit']
                        item['junit'] = parse_junit(junit_path, include_digest=True)
                        junit_sha256 = item['junit'].pop('sha256', None)
                        if item['junit']['state'] != 'passed' or junit_sha256 is None:
                            item['state'], item['reason'] = 'failed', 'junit_not_passed'
                        else:
                            item['junit_sha256'] = junit_sha256
                    if item['state'] == 'cancelled':
                        raise VerificationError('cancelled')
                item.update({'run_id': run_id, 'source_digest': canonical_digest(source)})
                results[gate_id] = item
                report['evidence'].append(item)
            if not progress:
                raise VerificationError('dependency_cycle')
        namespace_watch.assert_clean()
        after = collect_snapshot(root, base_ref, policy_root)
        if after != source:
            raise VerificationError('source_changed_during_verification')
        assert_mutation_epoch(epoch)
        decision = invoke_policy({'operation': 'evaluate', 'source': source,
                                  'run_id': run_id, 'source_digest': canonical_digest(source),
                                  'evidence': report['evidence']}, policy_root)
        namespace_watch.assert_clean()
        after_policy = collect_snapshot(root, base_ref, policy_root)
        if after_policy != source:
            raise VerificationError('source_changed_during_verification')
        assert_mutation_epoch(epoch)
        if decision.get('verdict') not in {'verified', 'blocked', 'failed'} or decision.get('required') != required:
            raise VerificationError('invalid_policy_decision')
        if not isinstance(decision.get('reasons'), list):
            raise VerificationError('invalid_policy_reasons')
        report['verdict'] = decision['verdict']
        report['reasons'] = decision['reasons']
        if report['verdict'] == 'verified' and report['reasons']:
            raise VerificationError('contradictory_policy_decision')
        report['created_ms'] = int(time.time() * 1000)
        report['required'] = required
        (evidence_dir / 'observations.json').write_text(json.dumps(report, indent=2) + '\n')
    except (VerificationError, OSError, ValueError) as error:
        report['verdict'] = 'blocked'
        report['reasons'] = [str(error)[:256]]
    finally:
        if namespace_watch is not None:
            namespace_watch.close()
    return report


def through_expert(result: dict[str, Any], root: Path, base_ref: str,
                   policy_root: Path, session: str, operation: str) -> dict[str, Any]:
    sys.path.insert(0, str(policy_root))
    from verification.zara_verifier_expert import invoke_verifier_expert

    class HostObservations:
        def plan(self):
            return result

        def snapshot(self):
            return collect_snapshot(root, base_ref, policy_root)

        def report(self):
            return result

    expert_operation = 'verify.plan' if operation == 'plan' else 'verify.assert'
    assertion = invoke_verifier_expert(HostObservations(), policy_root, session, expert_operation)
    result['expert'] = assertion
    if operation == 'run' and not (
        assertion['verdict'] == 'succeeded' and assertion['data'].get('verified') is True
        and type(assertion['usage'].get('model_calls')) is int
        and assertion['usage']['model_calls'] == 0
        and type(assertion['usage'].get('provider_calls')) is int
        and assertion['usage']['provider_calls'] == 0
    ):
        result['verdict'] = 'blocked'
        result['reasons'] = result.get('reasons', []) + ['expert_assertion_blocked']
    return result


def main() -> int:
    parser = argparse.ArgumentParser(description='ZARA-VERIFY/1 host verifier')
    parser.add_argument('operation', choices=['plan', 'run', 'snapshot'])
    parser.add_argument('--root', type=Path, default=Path.cwd())
    parser.add_argument('--base', default='origin/release/0.3.x')
    parser.add_argument('--session', default='local-cli')
    args = parser.parse_args()
    policy_root = Path(__file__).resolve().parents[1]
    try:
        if not args.session or len(args.session) > 128:
            raise VerificationError('invalid_session')
        if args.operation == 'run':
            result = run_verification(args.root.resolve(), args.base, policy_root, args.session)
        elif args.operation == 'plan':
            result = plan_verification(args.root.resolve(), args.base, policy_root)
        else:
            result = {'protocol': PROTOCOL, 'source': collect_snapshot(args.root, args.base, policy_root)}
        if args.operation != 'snapshot':
            result = through_expert(result, args.root.resolve(), args.base, policy_root, args.session, args.operation)
    except (VerificationError, OSError, ValueError, ImportError) as error:
        result = {'protocol': PROTOCOL, 'verdict': 'blocked', 'reasons': [str(error)[:256]]}
    print(json.dumps(result, sort_keys=True))
    return 2 if result.get('verdict') in {'blocked', 'failed'} else 0


if __name__ == '__main__':
    raise SystemExit(main())