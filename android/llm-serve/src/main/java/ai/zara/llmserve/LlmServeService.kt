package ai.zara.llmserve

import ai.zara.app.localai.LocalModelBackend
import ai.zara.app.localai.LocalModelFormat
import ai.zara.app.localai.LocalModelMetadata
import ai.zara.app.localai.LocalModelQuantization
import android.app.Notification
import android.app.NotificationChannel
import android.app.NotificationManager
import android.app.Service
import android.content.Intent
import android.net.Uri
import android.os.Build
import android.os.IBinder
import java.security.MessageDigest
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

class LlmServeService : Service() {
    private lateinit var engine: LlmServeEngine
    private lateinit var server: OllamaLoopbackServer
    private val worker: ExecutorService = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-llm-serve-service").apply { isDaemon = true }
    }

    override fun onCreate() {
        super.onCreate()
        engine = LlmServeEngine(this)
        server = OllamaLoopbackServer(engine)
        ensureNotificationChannel()
        persistStatus("stopped")
    }

    override fun onBind(intent: Intent?): IBinder? = null

    override fun onStartCommand(intent: Intent?, flags: Int, startId: Int): Int {
        when (intent?.action ?: ACTION_START) {
            ACTION_START -> startServer()
            ACTION_STOP -> stopServerAndSelf()
            ACTION_IMPORT_MODEL -> {
                val importIntent = intent
                if (importIntent == null) {
                    persistStatus("failed: missing import intent")
                } else {
                    importModel(importIntent)
                }
            }
            else -> persistStatus("error: unsupported action")
        }
        return START_STICKY
    }

    private fun startServer() {
        startForeground(
            NOTIFICATION_ID,
            notification("Starting local LLM server"),
        )
        worker.execute {
            try {
                engine.loadActiveModel()
                server.start()
                val active = engine.activeModel()
                val model = if (active == null) {
                    "no model"
                } else {
                    active.id + ":" + active.version
                }
                val status =
                    "ready • http://" +
                        OllamaLoopbackServer.LOOPBACK_HOST +
                        ":" +
                        OllamaLoopbackServer.DEFAULT_PORT +
                        " • " +
                        model
                persistStatus(status)
                notifyStatus(status)
            } catch (error: Throwable) {
                val status = "failed: " + boundedMessage(error)
                persistStatus(status)
                notifyStatus(status)
            }
        }
    }

    private fun importModel(intent: Intent) {
        val uri = intent.data
        if (uri == null) {
            persistStatus("failed: missing model URI")
            return
        }
        startForeground(
            NOTIFICATION_ID,
            notification("Importing local model"),
        )
        worker.execute {
            try {
                val metadata = metadataFrom(intent, uri)
                engine.install(uri, metadata)
                if (!server.isRunning()) server.start()
                val status =
                    "ready • imported " + metadata.id + ":" + metadata.version
                persistStatus(status)
                notifyStatus(status)
            } catch (error: Throwable) {
                val status = "failed: " + boundedMessage(error)
                persistStatus(status)
                notifyStatus(status)
            }
        }
    }

    private fun metadataFrom(intent: Intent, uri: Uri): LocalModelMetadata {
        val id = requireNotNull(intent.getStringExtra(EXTRA_MODEL_ID)) {
            "Model id is required"
        }
        val version = requireNotNull(intent.getStringExtra(EXTRA_MODEL_VERSION)) {
            "Model version is required"
        }
        val quantization = LocalModelQuantization.requireKnown(
            requireNotNull(intent.getStringExtra(EXTRA_QUANTIZATION)) {
                "Quantization is required"
            }
        )
        val backend = LocalModelBackend.valueOf(
            requireNotNull(intent.getStringExtra(EXTRA_BACKEND)) {
                "Backend is required"
            }
        )
        val contextTokens = intent.getIntExtra(EXTRA_CONTEXT_TOKENS, 0)
        require(contextTokens in 128..131_072) {
            "Context tokens are invalid"
        }
        return LocalModelMetadata(
            id = id,
            version = version,
            quantization = quantization,
            sha256 = sha256(uri),
            maxContextTokens = contextTokens,
            backend = backend,
            format = LocalModelFormat.LITERT_LM,
        )
    }

    private fun sha256(uri: Uri): String {
        val digest = MessageDigest.getInstance("SHA-256")
        val input = contentResolver.openInputStream(uri)
            ?: throw IllegalArgumentException("Selected model cannot be opened")
        input.buffered().use { stream ->
            val buffer = ByteArray(DEFAULT_BUFFER_SIZE)
            var total = 0L
            while (true) {
                val read = stream.read(buffer)
                if (read < 0) break
                if (read == 0) continue
                total += read
                require(total <= MAX_MODEL_BYTES) {
                    "Model exceeds the supported size limit"
                }
                digest.update(buffer, 0, read)
            }
            require(total > 0L) { "Model is empty" }
        }
        return digest.digest().joinToString("") { byte -> "%02x".format(byte) }
    }

    private fun stopServerAndSelf() {
        worker.execute {
            runCatching { server.close() }
            persistStatus("stopped")
            stopForeground(STOP_FOREGROUND_REMOVE)
            stopSelf()
        }
    }

    override fun onDestroy() {
        runCatching { server.close() }
        runCatching { engine.close() }
        worker.shutdownNow()
        persistStatus("stopped")
        super.onDestroy()
    }

    private fun ensureNotificationChannel() {
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.O) return
        val manager = getSystemService(NotificationManager::class.java)
        manager.createNotificationChannel(
            NotificationChannel(
                CHANNEL_ID,
                "Zara local LLM server",
                NotificationManager.IMPORTANCE_LOW,
            )
        )
    }

    private fun notification(text: String): Notification =
        Notification.Builder(this, CHANNEL_ID)
            .setContentTitle("Zara LLM Serve")
            .setContentText(text)
            .setSmallIcon(android.R.drawable.stat_sys_download_done)
            .setOngoing(true)
            .build()

    private fun notifyStatus(text: String) {
        getSystemService(NotificationManager::class.java)
            .notify(NOTIFICATION_ID, notification(text))
    }

    private fun persistStatus(value: String) {
        getSharedPreferences(PREFS, MODE_PRIVATE)
            .edit()
            .putString(KEY_STATUS, value.take(512))
            .apply()
    }

    private fun boundedMessage(error: Throwable): String =
        (error.message ?: error::class.java.simpleName).take(256)

    companion object {
        const val ACTION_START = "ai.zara.llmserve.START"
        const val ACTION_STOP = "ai.zara.llmserve.STOP"
        const val ACTION_IMPORT_MODEL = "ai.zara.llmserve.IMPORT_MODEL"

        const val EXTRA_MODEL_ID = "model_id"
        const val EXTRA_MODEL_VERSION = "model_version"
        const val EXTRA_QUANTIZATION = "quantization"
        const val EXTRA_BACKEND = "backend"
        const val EXTRA_CONTEXT_TOKENS = "context_tokens"

        const val PREFS = "llm-serve-status"
        const val KEY_STATUS = "status"

        private const val CHANNEL_ID = "zara-llm-serve"
        private const val NOTIFICATION_ID = 11434
        private const val MAX_MODEL_BYTES = 8L * 1024 * 1024 * 1024
    }
}
