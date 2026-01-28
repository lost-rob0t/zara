"""
Base classes for tools and skills.

Defines abstract interfaces that all tools must implement,
and provides skill system for Python/Prolog integration.
"""

from abc import ABC, abstractmethod
from typing import Any, Dict, Optional
from enum import Enum
from dataclasses import dataclass


class SkillType(Enum):
    """Type of skill implementation."""
    PYTHON = "python"
    PROLOG = "prolog"
    HYBRID = "hybrid"


@dataclass
class SkillMetadata:
    """
    Metadata describing a skill.

    Used for tool registration and documentation.
    """
    name: str
    description: str
    skill_type: SkillType
    parameters: Dict[str, Any]


class BaseTool(ABC):
    """
    Abstract base class for agent tools.

    Tools are functions the LLM can call during conversation.
    Each tool must define its name, description, and execution logic.
    """

    @property
    @abstractmethod
    def name(self) -> str:
        """
        Tool name (used by LLM to call it).

        Should be lowercase with underscores, e.g. "get_weather"
        """
        pass

    @property
    @abstractmethod
    def description(self) -> str:
        """
        Tool description for LLM.

        Should explain what the tool does and what parameters it expects.
        Be specific about input format and return value.
        """
        pass

    @property
    def parameters(self) -> Dict[str, Any]:
        """
        Tool parameter schema (JSON Schema format).

        Default implementation returns empty dict (no parameters).
        Override to specify parameters.

        Example:
            {
                "location": {
                    "type": "string",
                    "description": "City name"
                },
                "units": {
                    "type": "string",
                    "enum": ["celsius", "fahrenheit"],
                    "description": "Temperature units"
                }
            }
        """
        return {}

    @abstractmethod
    def execute(self, **kwargs) -> str:
        """
        Execute the tool with given parameters.

        Args:
            **kwargs: Tool parameters as keyword arguments

        Returns:
            Tool result as string (returned to LLM)

        Raises:
            Exception: On tool execution failure
        """
        pass


class BaseSkill(ABC):
    """
    Abstract base class for skills.

    Skills are independent implementations that can be called directly
    or wrapped as tools. They can be Python, Prolog, or hybrid.
    """

    @property
    @abstractmethod
    def metadata(self) -> SkillMetadata:
        """Skill metadata."""
        pass

    @abstractmethod
    def execute(self, **kwargs) -> Any:
        """
        Execute skill with given parameters.

        Args:
            **kwargs: Skill parameters

        Returns:
            Skill result (type depends on skill)
        """
        pass


class PythonSkill(BaseSkill):
    """
    Pure Python skill implementation.

    Subclass this for skills implemented entirely in Python.
    """

    @property
    def metadata(self) -> SkillMetadata:
        return SkillMetadata(
            name=self.name,
            description=self.description,
            skill_type=SkillType.PYTHON,
            parameters=self.parameters
        )

    @property
    @abstractmethod
    def name(self) -> str:
        pass

    @property
    @abstractmethod
    def description(self) -> str:
        pass

    @property
    def parameters(self) -> Dict[str, Any]:
        return {}


class PrologSkill(BaseSkill):
    """
    Prolog-backed skill implementation.

    Wraps Prolog predicates as skills. Converts Python kwargs to
    Prolog arguments and executes queries.
    """

    def __init__(self, prolog_engine):
        """
        Initialize Prolog skill.

        Args:
            prolog_engine: PrologEngine instance
        """
        self.prolog_engine = prolog_engine

    @property
    def metadata(self) -> SkillMetadata:
        return SkillMetadata(
            name=self.name,
            description=self.description,
            skill_type=SkillType.PROLOG,
            parameters=self.parameters
        )

    @property
    @abstractmethod
    def name(self) -> str:
        pass

    @property
    @abstractmethod
    def description(self) -> str:
        pass

    @property
    @abstractmethod
    def prolog_predicate(self) -> str:
        """Prolog predicate name to call."""
        pass

    @property
    def parameters(self) -> Dict[str, Any]:
        return {}

    def execute(self, **kwargs) -> Any:
        """
        Execute Prolog predicate with given parameters.

        Converts kwargs to Prolog query and executes it.
        """
        # Build Prolog query from parameters
        args = ", ".join(str(v) for v in kwargs.values())
        query = f"{self.prolog_predicate}({args})"

        # Execute query
        result = self.prolog_engine.query_once(query)
        return result
