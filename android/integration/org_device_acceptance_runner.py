"""Run Org device acceptance with a bounded UIAutomator stream fallback.

Hosted API-35 emulators can report a successful file-backed hierarchy dump while
failing to materialize the requested guest file. Keep the shared Device behavior
strict, but let this evidence lane retry that exact failure once by streaming the
same UIAutomator hierarchy over adb exec-out.
"""

from __future__ import annotations

import xml.etree.ElementTree as ET

import org_device_acceptance as org_acceptance
from device_acceptance import Device


class OrgEvidenceDevice(Device):
    """Device adapter for the Org evidence lane only."""

    def nodes(self):
        try:
            return tuple(super().nodes())
        except AssertionError as error:
            if "UIAutomator did not create" not in str(error):
                raise

            hierarchy = self.adb("exec-out", "uiautomator", "dump", "/dev/tty")
            start = hierarchy.find("<?xml")
            end = hierarchy.rfind("</hierarchy>")
            if start < 0 or end < 0:
                raise AssertionError(
                    "UIAutomator direct-stream fallback produced no XML hierarchy"
                ) from error

            end += len("</hierarchy>")
            try:
                return tuple(ET.fromstring(hierarchy[start:end]).iter("node"))
            except ET.ParseError as parse_error:
                raise AssertionError(
                    "UIAutomator direct-stream fallback produced malformed XML"
                ) from parse_error


def main() -> None:
    org_acceptance.Device = OrgEvidenceDevice
    org_acceptance.main()


if __name__ == "__main__":
    main()
