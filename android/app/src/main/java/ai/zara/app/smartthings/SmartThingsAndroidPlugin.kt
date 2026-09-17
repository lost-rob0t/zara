package ai.zara.app.smartthings

import ai.zara.app.auth.AndroidKeystoreCredentialCipher
import ai.zara.app.runtime.LocalQueryResult
import android.content.Context
import java.io.File
import java.util.concurrent.CompletableFuture

class SmartThingsAndroidPlugin(
    private val queryProlog: (String) -> CompletableFuture<LocalQueryResult>,
    private val actor: SmartThingsPluginActor,
    private val credentials: SmartThingsCredentialStore,
) : AutoCloseable {
    fun devices(): CompletableFuture<SmartThingsPluginReply> =
        run("smartthings_devices(Result)")

    fun status(alias: String): CompletableFuture<SmartThingsPluginReply> =
        run("smartthings_status(${requireAlias(alias)}, Result)")

    fun turnOn(alias: String): CompletableFuture<SmartThingsPluginReply> =
        run("smartthings_on(${requireAlias(alias)}, Result)")

    fun turnOff(alias: String): CompletableFuture<SmartThingsPluginReply> =
        run("smartthings_off(${requireAlias(alias)}, Result)")

    fun setLevel(alias: String, level: Int): CompletableFuture<SmartThingsPluginReply> {
        require(level in 0..100) { "SmartThings level must be between 0 and 100" }
        return run("smartthings_set_level(${requireAlias(alias)}, $level, Result)")
    }

    fun credentialState(): SmartThingsCredentialState = credentials.state()

    fun connectPersonalAccessToken(token: String, expiresAtEpochMillis: Long) {
        credentials.savePersonalAccessToken(token, expiresAtEpochMillis)
    }

    fun disconnect(): Boolean = credentials.clear()

    private fun run(query: String): CompletableFuture<SmartThingsPluginReply> =
        queryProlog(query).thenCompose { result ->
            actor.dispatch(result.terms).thenApply { reply ->
                reply ?: SmartThingsPluginReply("No SmartThings rule matched.", false)
            }
        }

    private fun requireAlias(value: String): String {
        require(value.matches(ALIAS)) { "SmartThings alias must be a bounded Prolog atom" }
        return value
    }

    override fun close() {
        actor.close()
    }

    companion object {
        private val ALIAS = Regex("[a-z][a-z0-9_]{0,63}")

        fun create(
            context: Context,
            queryProlog: (String) -> CompletableFuture<LocalQueryResult>,
        ): SmartThingsAndroidPlugin {
            val applicationContext = context.applicationContext
            val credentials = SmartThingsCredentialStore(
                file = File(
                    applicationContext.noBackupFilesDir,
                    "zara/smartthings/credential.bin",
                ),
                cipher = AndroidKeystoreCredentialCipher("zara.smartthings.wrap.v1"),
            )
            val gateway = SmartThingsApiGateway(StoredSmartThingsTokenProvider(credentials))
            return SmartThingsAndroidPlugin(
                queryProlog = queryProlog,
                actor = SmartThingsPluginActor(gateway),
                credentials = credentials,
            )
        }
    }
}
