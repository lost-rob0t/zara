from __future__ import annotations

import unittest

from scripts.org_issue_sync import (
    MANAGED_START,
    OrgTask,
    ValidationError,
    marker,
    merge_managed_body,
    parse_org_text,
    sync_task,
    validate_tasks,
)


REQUIRED = {
    "ID": "task-1",
    "ISSUE_SYNC": "auto",
    "RAGE_SLICE": "initial",
    "RAGE_PARENT": "#1192",
    "RAGE_PRIORITY": "P1",
    "RAGE_SCOPE": "contract",
    "RAGE_NON_GOALS": "no fork",
    "RAGE_INVARIANTS": "one authority",
    "RAGE_DEPENDS": "#1188",
    "RAGE_BRANCH": "rage/task-1",
    "RAGE_RESEARCH": "current code",
    "RAGE_ACCEPTANCE": "fixture passes",
    "RAGE_TDD": "red then green",
    "RAGE_ARTIFACTS": "tests docs",
    "RAGE_GATE": "nix flake check",
}


def task(state="TODO", **overrides):
    props = dict(REQUIRED)
    props.update(overrides)
    return OrgTask(state, "Example", props, "Body", 1)


class FakeClient:
    def __init__(self, issue=None):
        self.issue = issue
        self.created = []
        self.updated = []

    def get_issue(self, repo, number):
        if self.issue is None:
            raise AssertionError("unexpected get")
        return dict(self.issue)

    def find_issue_by_marker(self, repo, task_id):
        return dict(self.issue) if self.issue is not None else None

    def create_issue(self, repo, title, body, labels):
        created = {
            "number": 42,
            "title": title,
            "body": body,
            "state": "open",
        }
        self.created.append((repo, title, body, labels))
        self.issue = created
        return created

    def update_issue(self, repo, number, payload):
        self.updated.append((repo, number, payload))
        self.issue.update(payload)
        return dict(self.issue)


class OrgIssueSyncTest(unittest.TestCase):
    def test_parse_idea_and_todo(self):
        parsed = parse_org_text(
            """* Idea Inbox
** IDEA Deferred
:PROPERTIES:
:ID: deferred
:END:
No issue.
** TODO Active
:PROPERTIES:
:ID: active
:ISSUE_SYNC: auto
:END:
Ship it.
"""
        )
        self.assertEqual(["IDEA", "TODO"], [item.state for item in parsed])
        self.assertEqual("active", parsed[1].task_id)

    def test_complete_initial_slice_validates(self):
        validate_tasks([task()])

    def test_missing_initial_slice_property_rejected(self):
        bad = dict(REQUIRED)
        del bad["RAGE_GATE"]
        with self.assertRaises(ValidationError):
            validate_tasks([OrgTask("TODO", "Bad", bad, "", 7)])

    def test_duplicate_id_rejected(self):
        with self.assertRaises(ValidationError):
            validate_tasks([task(), task(RAGE_PRIORITY="P2")])

    def test_idea_never_syncs(self):
        client = FakeClient()
        result = sync_task(task(state="IDEA"), client, "lost-rob0t/zara", "ideas.org")
        self.assertEqual("ignored", result)
        self.assertFalse(client.created)

    def test_create_then_update_is_idempotent(self):
        client = FakeClient()
        created = sync_task(task(), client, "lost-rob0t/zara", "ideas.org")
        self.assertEqual("created lost-rob0t/zara#42", created)
        self.assertIn(marker("task-1"), client.issue["body"])
        self.assertIn(MANAGED_START, client.issue["body"])

        unchanged = sync_task(task(), client, "lost-rob0t/zara", "ideas.org")
        self.assertEqual("unchanged lost-rob0t/zara#42", unchanged)

        updated = sync_task(
            task(RAGE_SCOPE="new scope"),
            client,
            "lost-rob0t/zara",
            "ideas.org",
        )
        self.assertEqual("updated lost-rob0t/zara#42", updated)
        self.assertIn("new scope", client.issue["body"])

    def test_done_closes_existing_issue(self):
        existing = {
            "number": 9,
            "title": "Example",
            "body": marker("task-1"),
            "state": "open",
        }
        client = FakeClient(existing)
        result = sync_task(
            task(state="DONE", ISSUE_NUMBER="9"),
            client,
            "lost-rob0t/zara",
            "ideas.org",
        )
        self.assertEqual("closed lost-rob0t/zara#9", result)
        self.assertEqual("closed", client.issue["state"])
        self.assertEqual("completed", client.issue["state_reason"])

    def test_existing_manual_body_is_preserved(self):
        existing = marker("task-1") + "\n\nManual issue details."
        merged = merge_managed_body(existing, task(), "ideas.org")
        self.assertIn("Manual issue details.", merged)
        self.assertIn(MANAGED_START, merged)


if __name__ == "__main__":
    unittest.main()
