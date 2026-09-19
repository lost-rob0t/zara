package ai.zara.app.ui

import ai.zara.app.runtime.RuntimeHealth
import ai.zara.app.runtime.RuntimeLocality
import ai.zara.app.runtime.RuntimeRegistrySnapshot

data class RuntimeSettingsRuntimeRow(
    val runtimeId: String,
    val displayName: String,
    val runtimeVersion: String,
    val implementationVersion: String,
    val health: RuntimeHealth,
    val locality: RuntimeLocality,
    val profiles: List<String>,
    val selectable: Boolean,
    val selected: Boolean,
)

/**
 * UI-only projection of the canonical ZARA-RUNTIME/1 registry snapshot.
 *
 * This deliberately contains no Auto/Local/Remote routing-policy state and no
 * product-specific runtime rows. If discovery did not place an installed
 * runtime in [RuntimeRegistrySnapshot.descriptors], this projection cannot
 * manufacture it.
 */
fun RuntimeRegistrySnapshot.runtimeSettingsRows(): List<RuntimeSettingsRuntimeRow> =
    descriptors
        .asSequence()
        .filter { it.installed }
        .sortedBy { it.id }
        .map { descriptor ->
            RuntimeSettingsRuntimeRow(
                runtimeId = descriptor.id,
                displayName = descriptor.displayName,
                runtimeVersion = descriptor.runtimeVersion,
                implementationVersion = descriptor.implementationVersion,
                health = descriptor.health,
                locality = descriptor.locality,
                profiles = descriptor.profiles.toList(),
                selectable = descriptor.selectable,
                selected = selection?.runtimeId == descriptor.id,
            )
        }
        .toList()
