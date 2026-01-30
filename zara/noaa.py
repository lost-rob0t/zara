"""NOAA weather tool integration."""

from __future__ import annotations

from typing import Optional

import requests
from langchain_core.tools import StructuredTool
from pydantic import BaseModel, Field


def _get_noaa_config() -> dict:
    try:
        from .config import get_config
    except Exception:
        return {}

    config = get_config()
    return config.get_section("noaa") or {}


def _normalize_coordinate(value: Optional[float]) -> Optional[float]:
    if value is None:
        return None
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


class NoaaWeatherArgs(BaseModel):
    days: int = Field(
        default=1,
        ge=1,
        le=7,
        description="How many forecast periods to return (1-7).",
    )
    units: str = Field(
        default="auto",
        description="Units to display: 'us', 'si', or 'auto' for NOAA default.",
    )


def _build_headers(user_agent: Optional[str]) -> dict:
    agent = user_agent or "ZarathushtraWeather/1.0"
    return {"User-Agent": agent, "Accept": "application/geo+json"}


def _fetch_json(url: str, headers: dict) -> dict:
    response = requests.get(url, headers=headers, timeout=10)
    response.raise_for_status()
    return response.json()


def _format_period(period: dict, units: str) -> str:
    name = period.get("name", "")
    temperature = period.get("temperature")
    unit = period.get("temperatureUnit", "")
    wind_speed = period.get("windSpeed", "")
    wind_direction = period.get("windDirection", "")
    forecast = period.get("shortForecast", "")

    parts = []
    if name:
        parts.append(f"{name}:")
    if temperature is not None:
        parts.append(f"{temperature}{unit}")
    if forecast:
        parts.append(forecast)
    if wind_speed and wind_direction:
        parts.append(f"Wind {wind_speed} {wind_direction}")

    if not parts:
        return "No forecast data available."

    line = " ".join(parts)
    if units.lower() == "si":
        return f"{line} (NOAA reports in US units)"
    return line


def _resolve_coordinates(config: dict) -> tuple[Optional[float], Optional[float]]:
    lat = _normalize_coordinate(config.get("default_latitude"))
    lon = _normalize_coordinate(config.get("default_longitude"))
    return lat, lon


def _missing_coordinates_message() -> str:
    return (
        "Missing latitude/longitude in config. Ask the user to set defaults in "
        "[noaa] (default_latitude, default_longitude)."
    )


def _get_forecast(
    latitude: float,
    longitude: float,
    units: str,
    user_agent: Optional[str],
    days: int,
) -> str:
    headers = _build_headers(user_agent)
    points_url = f"https://api.weather.gov/points/{latitude},{longitude}"
    points_data = _fetch_json(points_url, headers)

    properties = points_data.get("properties") or {}
    forecast_url = properties.get("forecast")
    if not forecast_url:
        return "No forecast endpoint found for those coordinates."

    forecast_data = _fetch_json(forecast_url, headers)
    periods = (forecast_data.get("properties") or {}).get("periods") or []
    if not periods:
        return "No forecast periods returned by NOAA."

    max_periods = max(1, min(int(days), 7))
    lines = [_format_period(period, units) for period in periods[:max_periods]]
    return "\n".join(lines)


def get_noaa_weather(days: int = 1, units: str = "auto") -> str:
    """
    Get NOAA forecast using configured defaults.

    If coordinates are missing, the tool returns a prompt for the user
    to set defaults in the config.
    """
    config = _get_noaa_config()
    resolved_lat, resolved_lon = _resolve_coordinates(config)
    if resolved_lat is None or resolved_lon is None:
        return _missing_coordinates_message()

    user_agent = config.get("user_agent")
    try:
        return _get_forecast(resolved_lat, resolved_lon, units, user_agent, days)
    except requests.RequestException as exc:
        return f"NOAA request failed: {exc}"
    except Exception as exc:
        return f"Failed to fetch NOAA forecast: {exc}"


def build_noaa_weather_tool() -> StructuredTool:
    return StructuredTool.from_function(
        func=get_noaa_weather,
        name="get_noaa_weather",
        description=(
            "Fetch the latest NOAA forecast using the configured default coordinates. "
            "If defaults are missing, tell the user to set [noaa].default_latitude and "
            "[noaa].default_longitude in config.toml. This tool calls NOAA /points to "
            "resolve the forecast URL and returns 1-7 forecast periods (temperature, "
            "short forecast, wind)."
        ),
        args_schema=NoaaWeatherArgs,
    )
