"""Zarathushtra Pets — animated desktop companion system.

The pet UI is decoupled from runtime concerns: it consumes normalized
``PetState`` values (idle/running/needs-input/ready/blocked) produced by
``PetStateActor`` from domain events emitted by the runtime. Qt/PySide6 is
imported lazily so the core (state, manifest, importer, animation) is fully
testable in headless environments.
"""

from __future__ import annotations

from .state import PetState, ActivityKind, Activity, PetStatusDeriver

__all__ = ["PetState", "ActivityKind", "Activity", "PetStatusDeriver"]