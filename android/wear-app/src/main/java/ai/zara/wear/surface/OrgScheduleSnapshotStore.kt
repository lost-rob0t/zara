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

    @Synchronized
    fun replace(context: Context, snapshot: OrgScheduleSnapshot): Boolean {
        val current = read(context)
        if (!shouldAcceptOrgScheduleSnapshot(current, snapshot)) return false
        if (current == snapshot) return true

        val committed = context
            .getSharedPreferences(PREFS, Context.MODE_PRIVATE)
            .edit()
            .putString(KEY_SNAPSHOT, OrgScheduleSnapshotCodec.encode(snapshot))
            .commit()
        check(committed) { "Failed to persist Org schedule snapshot" }
        return true
    }

    fun replaceEncoded(context: Context, encoded: String): Boolean {
        val snapshot = OrgScheduleSnapshotCodec.decode(encoded) ?: return false
        return replace(context, snapshot)
    }
}
