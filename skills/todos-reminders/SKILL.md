---
name: todos-reminders
description: Manage Zara one-time todos and reminders, including list, edit, complete, search, schedule, reopen, and export.
metadata:
  zara-schema: "1"
  zara-domain: "todos"
  zara-selectors: "todo todos reminder reminders one-time-task todo-list"
  zara-priority: "89"
  zara-max-tokens: "950"
  zara-paths: "zara/todo_skills.py zara/agent/tools/todo_tools.py"
  zara-always-on: "false"
---
# Todos and reminders

Use Zara's todo tools for ordinary one-time task tracking.

- `add_todo` captures a new todo.
- `list_todos` lists active todos with optional status filtering.
- `search_todos` finds a todo before an edit when the ID is not already known.
- `edit_todo`, `complete_todo`, and `reopen_todo` mutate an existing todo by ID.
- `schedule_todo` attaches a one-time schedule to a todo.
- `export_todos` exports the todo store in the supported format.
- Do not use the todo store for recurring autonomous jobs. Recurring/periodic requests belong to agent mode when that capability is enabled.
- If the built-in todo subsystem is disabled, do not silently use it; an external plugin such as an Org-backed provider may be authoritative instead.
- Preserve exact times and named people/details supplied by the user when creating or editing scheduled tasks.
