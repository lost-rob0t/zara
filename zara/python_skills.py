"""
Python skills registry for Zara.

These skills are invoked when Prolog resolves an intent to python(Name).
"""

from typing import Any, Callable, Dict, List

from .noaa import get_noaa_weather


def say_hello(args: List[Any]) -> str:
    name = args[0] if args else "there"
    return f"Hello, {name}!"


def noaa_weather(args: List[Any]) -> str:
    return get_noaa_weather()


class PythonSkillRegistry:
    def __init__(self) -> None:
        self._skills: Dict[str, Callable[[List[Any]], str]] = {
            "say_hello": say_hello,
            "noaa_weather": noaa_weather,
        }

    def register(self, name: str, func: Callable[[List[Any]], str]) -> None:
        self._skills[name] = func

    def execute(self, skill_name: str, args: List[Any]) -> str:
        func = self._skills.get(skill_name)
        if func is None:
            raise NotImplementedError(f"Python skill '{skill_name}' is not implemented")
        return func(args)

    def list_skills(self) -> List[str]:
        return sorted(self._skills.keys())


python_skills = PythonSkillRegistry()
