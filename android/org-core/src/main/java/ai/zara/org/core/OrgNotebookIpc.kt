package ai.zara.org.core

/** Narrow same-signer IPC contract between the Org notebook APK and Zara runtime owner. */
object OrgNotebookIpc {
    const val ZARA_PACKAGE = "ai.zara.app"
    const val SERVICE_CLASS = "ai.zara.app.org.OrgNotebookExecutionService"
    const val PERMISSION = "ai.zara.permission.EXECUTE_NOTEBOOK"

    const val MSG_RUN = 1
    const val MSG_CANCEL = 2
    const val MSG_RESULT = 3

    const val KEY_REQUEST_ID = "request_id"
    const val KEY_DOCUMENT_ID = "document_id"
    const val KEY_SOURCE_REVISION = "source_revision"
    const val KEY_BLOCK_HASH = "block_hash"
    const val KEY_LANGUAGE = "language"
    const val KEY_BODY = "body"
    const val KEY_PRINCIPAL = "principal"
    const val KEY_DEADLINE_EPOCH_MS = "deadline_epoch_ms"

    const val KEY_STATUS = "status"
    const val KEY_STDOUT = "stdout"
    const val KEY_STDERR = "stderr"
    const val KEY_DURATION_MS = "duration_ms"
    const val KEY_RUNTIME_GENERATION = "runtime_generation"
    const val KEY_STRUCTURED_VALUE = "structured_value"

    const val STATUS_SUCCEEDED = "succeeded"
    const val STATUS_FAILED = "failed"
    const val STATUS_CANCELLED = "cancelled"
}
