from __future__ import annotations

import ast
import inspect

import zara.org_automation_receipt as receipt


_FORBIDDEN_IMPORT_ROOTS = {
    "asyncio",
    "os",
    "pathlib",
    "shelve",
    "sqlite3",
    "subprocess",
    "threading",
}
_FORBIDDEN_CLASS_SUFFIXES = (
    "Executor",
    "Registry",
    "Repository",
    "Runtime",
    "Scheduler",
    "Store",
    "SyncEngine",
)
_FORBIDDEN_FUNCTIONS = {
    "execute",
    "load",
    "persist",
    "run_command",
    "save",
    "schedule",
    "sync",
}
_EXPECTED_PUBLIC_API = {
    "AutomationReceiptError",
    "AutomationRunEvent",
    "AutomationRunProjection",
    "reduce_automation_run",
}


def test_receipt_module_remains_a_pure_derived_state_boundary() -> None:
    """Do not let this projection grow a second runtime/store/scheduler authority."""

    tree = ast.parse(inspect.getsource(receipt))
    imported_roots: set[str] = set()
    class_names: set[str] = set()
    function_names: set[str] = set()

    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            imported_roots.update(alias.name.split(".", 1)[0] for alias in node.names)
        elif isinstance(node, ast.ImportFrom) and node.module:
            imported_roots.add(node.module.split(".", 1)[0])
        elif isinstance(node, ast.ClassDef):
            class_names.add(node.name)
        elif isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            function_names.add(node.name)

    assert imported_roots.isdisjoint(_FORBIDDEN_IMPORT_ROOTS)
    assert not {
        name for name in class_names if name.endswith(_FORBIDDEN_CLASS_SUFFIXES)
    }
    assert function_names.isdisjoint(_FORBIDDEN_FUNCTIONS)
    assert set(receipt.__all__) == _EXPECTED_PUBLIC_API
