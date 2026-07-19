#!/usr/bin/env python3
"""
Stateful Prolog Engine Wrapper
-------------------------------
Maintains persistent connection to SWI-Prolog via PySWIP.
Handles queries, state management, and multi-solution iteration.
"""

import logging
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, Iterator, List, Optional

try:
    from pyswip import Prolog, Query
    from pyswip.core import PL_get_atom_chars
except ImportError as e:
    raise ImportError(
        "PySWIP not installed. Install with: pip install pyswip"
    ) from e


@dataclass(frozen=True)
class IntentResult:
    kind: str
    name: str
    args: List[Any]


def _compound_parts(value: Any) -> Optional[tuple[str, List[Any]]]:
    if isinstance(value, str) and value.endswith(")"):
        separator = value.find("(")
        if separator > 0:
            return value[:separator], [value[separator + 1:-1]]

    name = getattr(value, "name", None)
    args = getattr(value, "args", None)
    if isinstance(name, str) and args is not None:
        return name, list(args)
    return None


def adapt_intent_result(result: Dict[str, Any]) -> IntentResult:
    value = result.get("Intent")
    args = list(result.get("Args", []))
    compound = _compound_parts(value)
    if compound is None:
        return IntentResult("prolog", str(value), args)

    functor, values = compound
    inner = values[0] if values else ""
    if functor == "python":
        return IntentResult("python", str(inner), args)
    if functor == "pending":
        return IntentResult("pending", str(inner), args)
    return IntentResult("prolog", str(value), args)


class PrologEngine:
    """Stateful wrapper around PySWIP for interactive queries"""
    
    def __init__(self, main_file: Optional[Path] = None):
        self.prolog = Prolog()
        self.loaded_files = set()
        self.logger = logging.getLogger(__name__)
        
        if main_file:
            self.consult(main_file)
    
    def consult(self, filepath: Path) -> bool:
        """Load a Prolog file into the engine"""
        try:
            filepath = Path(filepath).resolve()
            if not filepath.exists():
                self.logger.error(f"File not found: {filepath}")
                return False
            
            self.prolog.consult(str(filepath))
            self.loaded_files.add(filepath)
            self.logger.info(f"Loaded: {filepath}")
            return True
        except Exception as e:
            self.logger.error(f"Failed to consult {filepath}: {e}")
            return False
    
    def query_once(self, goal: str) -> Optional[Dict[str, Any]]:
        """Execute query and return first solution or None"""
        try:
            result = list(self.prolog.query(goal, maxresult=1))
            return result[0] if result else None
        except Exception as e:
            self.logger.error(f"Query failed: {goal} | {e}")
            return None
    
    def query_all(self, goal: str, max_solutions: int = 100) -> List[Dict[str, Any]]:
        """Execute query and return all solutions up to max"""
        try:
            return list(self.prolog.query(goal, maxresult=max_solutions))
        except Exception as e:
            self.logger.error(f"Query failed: {goal} | {e}")
            return []
    
    def query_iter(self, goal: str) -> Iterator[Dict[str, Any]]:
        """Execute query and yield solutions lazily"""
        try:
            q = Query(goal)
            while q.nextSolution():
                yield dict(q.solution())
            q.closeQuery()
        except Exception as e:
            self.logger.error(f"Query iteration failed: {goal} | {e}")
    
    def execute_command(self, input_text: str) -> bool:
        """
        Execute command through Prolog's command_loop:handle_command/1
        Returns True if successful
        """
        try:
            # Escape quotes in input
            escaped = input_text.replace("'", "\\'")
            goal = f"command_loop:handle_command('{escaped}')"
            
            result = self.query_once(goal)
            return result is not None
        except Exception as e:
            self.logger.error(f"Command execution failed: {input_text} | {e}")
            return False
    
    def assert_fact(self, fact: str) -> bool:
        """Add a fact to the Prolog database"""
        try:
            goal = f"assertz({fact})"
            self.query_once(goal)
            return True
        except Exception as e:
            self.logger.error(f"Assert failed: {fact} | {e}")
            return False
    
    def retract_fact(self, fact: str) -> bool:
        """Remove a fact from the Prolog database"""
        try:
            goal = f"retract({fact})"
            result = self.query_once(goal)
            return result is not None
        except Exception as e:
            self.logger.error(f"Retract failed: {fact} | {e}")
            return False
    
    def check_predicate(self, predicate: str) -> bool:
        """Check if a predicate is defined"""
        try:
            goal = f"current_predicate({predicate})"
            result = self.query_once(goal)
            return result is not None
        except Exception as e:
            return False
    
    def get_app_mapping(self, app_name: str) -> Optional[str]:
        """Query app_mapping/2 from config"""
        result = self.query_once(f"kb_config:app_mapping({app_name}, Cmd)")
        return result.get("Cmd") if result else None
    
    def resolve_intent(self, text: str, state: str = "passive") -> Optional[IntentResult]:
        """Resolve natural language to intent + args"""
        if state not in {"passive", "conversation", "dictation"}:
            raise ValueError(f"Unsupported intent state: {state}")
        escaped = text.replace("\\", "\\\\").replace('"', '\\"')
        goal = f'intent_resolver:resolve("{escaped}", {state}, Intent, Args)'
        self.logger.info(f"Intent query: {goal}")
        result = self.query_once(goal)
        self.logger.info(f"Intent result: {result}")
        return adapt_intent_result(result) if result else None

    def execute_intent(self, intent: str, args: List[Any]) -> bool:
        """Execute a resolved intent and report whether its handler succeeded."""
        result = self.query_once(f"commands:execute({intent}, {args})")
        return result is not None

    def is_conversation_stop(self, text: str, state: str = "conversation") -> bool:
        """Check whether text matches a conversation stop intent."""
        result = self.resolve_intent(text, state=state)
        if not result:
            return False
        return result.kind == "prolog" and result.name == "end_conversation"

    def dictation_active(self) -> bool:
        """Check whether dictation mode is currently active."""
        result = self.query_once("dictation:dictation_active")
        return result is not None

    
    def reload_config(self) -> bool:
        """Reload user configuration"""
        try:
            result = self.query_once("config_loader:reload_user_config")
            return result is not None
        except Exception as e:
            self.logger.error(f"Config reload failed: {e}")
            return False


def test_engine():
    """Test basic functionality"""
    logging.basicConfig(level=logging.INFO)
    
    # Assuming main.pl is in parent directory
    main_file = Path(__file__).parent.parent / "main.pl"
    
    if not main_file.exists():
        print(f"Error: {main_file} not found")
        return
    
    engine = PrologEngine(main_file)
    
    # Test intent resolution
    print("\n=== Testing Intent Resolution ===")
    result = engine.resolve_intent("open firefox")
    print(f"Resolved: {result}")
    
    # Test app mapping query
    print("\n=== Testing App Mapping ===")
    cmd = engine.get_app_mapping("terminal")
    print(f"Terminal command: {cmd}")
    
    # Test command execution
    print("\n=== Testing Command Execution ===")
    success = engine.execute_command("hello")
    print(f"Command succeeded: {success}")


if __name__ == "__main__":
    test_engine()
