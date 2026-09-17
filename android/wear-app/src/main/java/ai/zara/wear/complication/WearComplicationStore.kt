package ai.zara.wear.complication

import ai.zara.ui.complication.PrologComplicationTemplate
import ai.zara.ui.complication.PrologComplicationType
import android.content.Context

class WearComplicationStore(context: Context) {
    private val preferences = context.getSharedPreferences(PREFERENCES, Context.MODE_PRIVATE)

    fun save(template: PrologComplicationTemplate, revision: Long) {
        val safe = template.validated()
        preferences.edit()
            .clear()
            .putLong(KEY_REVISION, revision)
            .putString(KEY_ID, safe.id)
            .putString(KEY_TYPE, safe.type.atom)
            .putString(KEY_TEXT, safe.text)
            .putString(KEY_TITLE, safe.title.orEmpty())
            .putString(KEY_DESCRIPTION, safe.description)
            .apply {
                if (safe.type == PrologComplicationType.RANGED_VALUE) {
                    putFloat(KEY_RANGE_VALUE, requireNotNull(safe.rangeValue))
                    putFloat(KEY_RANGE_MIN, requireNotNull(safe.rangeMin))
                    putFloat(KEY_RANGE_MAX, requireNotNull(safe.rangeMax))
                }
            }
            .apply()
    }

    fun load(): StoredComplicationTemplate? {
        val id = preferences.getString(KEY_ID, null) ?: return null
        val type = preferences.getString(KEY_TYPE, null)
            ?.let { runCatching { PrologComplicationType.fromAtom(it) }.getOrNull() }
            ?: return null
        val text = preferences.getString(KEY_TEXT, null) ?: return null
        val title = preferences.getString(KEY_TITLE, null).orEmpty().ifBlank { null }
        val description = preferences.getString(KEY_DESCRIPTION, null) ?: text
        val template = PrologComplicationTemplate(
            id = id,
            type = type,
            text = text,
            title = title,
            description = description,
            rangeValue = preferences.takeIf { it.contains(KEY_RANGE_VALUE) }
                ?.getFloat(KEY_RANGE_VALUE, 0f),
            rangeMin = preferences.takeIf { it.contains(KEY_RANGE_MIN) }
                ?.getFloat(KEY_RANGE_MIN, 0f),
            rangeMax = preferences.takeIf { it.contains(KEY_RANGE_MAX) }
                ?.getFloat(KEY_RANGE_MAX, 0f),
        )
        return runCatching {
            StoredComplicationTemplate(
                template = template.validated(),
                revision = preferences.getLong(KEY_REVISION, 0L),
            )
        }.getOrNull()
    }

    data class StoredComplicationTemplate(
        val template: PrologComplicationTemplate,
        val revision: Long,
    )

    companion object {
        private const val PREFERENCES = "zara-prolog-complication-v1"
        private const val KEY_REVISION = "revision"
        private const val KEY_ID = "id"
        private const val KEY_TYPE = "type"
        private const val KEY_TEXT = "text"
        private const val KEY_TITLE = "title"
        private const val KEY_DESCRIPTION = "description"
        private const val KEY_RANGE_VALUE = "range_value"
        private const val KEY_RANGE_MIN = "range_min"
        private const val KEY_RANGE_MAX = "range_max"
    }
}
