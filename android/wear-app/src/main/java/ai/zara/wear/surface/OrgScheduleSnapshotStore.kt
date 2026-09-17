package ai.zara.wear.surface

import android.content.Context

object OrgScheduleSnapshotStore {
    private const val PREFS = "zara_org_schedule"
    private const val KEY_SNAPSHOT = "snapshot_v1"

    fun read(context: Context): OrgScheduleSnapshot =
        context
            .getSharedPreferences(PREFS, Context.MODE_PRIVATE)
            .getString(KEY_SNAPSHOT, null)
            ?.let(OrgScheduleSnapshotCodec::decode)
            ?: OrgScheduleSnapshot.EMPTY

    fun replace(context: Context, snapshot: OrgScheduleSnapshot) {
        val encoded = OrgScheduleSnapshotCodec.encode(snapshot)
        val committed = context
            .getSharedPreferences(PREFS, Context.MODE_PRIVATE)
            .edit()
            .putString(KEY_SNAPSHOT, encoded)
            .commit()
        check(committed) { "Failed to persist Org schedule snapshot" }
    }

    fun replaceEncoded(context: Context, encoded: String) {
        val snapshot = requireNotNull(OrgScheduleSnapshotCodec.decode(encoded)) {
            "Invalid Org schedule snapshot"
        }
        replace(context, snapshot)
    }
}
