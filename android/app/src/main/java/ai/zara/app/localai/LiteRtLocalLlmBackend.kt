package ai.zara.app.localai

import android.content.Context
import com.google.ai.edge.litertlm.Backend
import com.google.ai.edge.litertlm.Content
import com.google.ai.edge.litertlm.Conversation
import com.google.ai.edge.litertlm.ConversationConfig
import com.google.ai.edge.litertlm.Contents
import com.google.ai.edge.litertlm.Engine
import com.google.ai.edge.litertlm.EngineConfig
import com.google.ai.edge.litertlm.LogSeverity
import com.google.ai.edge.litertlm.Message
import com.google.ai.edge.litertlm.MessageCallback
import java.io.File
import java.util.concurrent.atomic.AtomicBoolean

class LiteRtLocalLlmBackend(
    context: Context,
) : LocalLlmBackend {
    private val appContext = context.applicationContext
    private val cacheDirectory = File(appContext.cacheDir, "zara/litertlm")

    @Volatile
    private var engine: Engine? = null

    @Volatile
    private var activeConversation: Conversation? = null

    override fun load(spec: LocalModelSpec) {
        require(spec.format == LocalModelFormat.LITERT_LM) {
            "Embedded LiteRT-LM provider cannot execute ${spec.format.wireName} models"
        }
        check(engine == null) { "LiteRT-LM engine is already loaded" }
        val model = File(spec.path)
        check(model.isFile) { "LiteRT-LM model file is missing" }
        check(cacheDirectory.mkdirs() || cacheDirectory.isDirectory) { "LiteRT-LM cache is unavailable" }
        Engine.setNativeMinLogSeverity(LogSeverity.ERROR)
        val config = EngineConfig(
            modelPath = model.absolutePath,
            backend = spec.backend.toLiteRtBackend(),
            visionBackend = spec.backend.toLiteRtBackend(),
            maxNumTokens = spec.maxContextTokens,
            cacheDir = cacheDirectory.absolutePath,
        )
        val next = Engine(config)
        try {
            next.initialize()
            engine = next
        } catch (error: Throwable) {
            runCatching { if (next.isInitialized()) next.close() }
            throw error
        }
    }

    override fun generate(
        request: LocalGenerationRequest,
        listener: LocalGenerationListener,
    ): LocalGenerationSession {
        val currentEngine = checkNotNull(engine) { "LiteRT-LM engine is not loaded" }
        check(activeConversation == null) { "LiteRT-LM generation is already active" }
        val conversation = currentEngine.createConversation(
            ConversationConfig(
                systemInstruction = Contents.of(
                    "You are Zara, an offline local assistant. Answer conversational requests directly. " +
                        "Never claim that you executed device actions. Device actions are authorized and executed by Zara's symbolic policy layer."
                ),
                tools = emptyList(),
                automaticToolCalling = false,
                maxOutputToken = request.maxOutputTokens,
            )
        )
        activeConversation = conversation
        val terminal = AtomicBoolean(false)
        val session = object : LocalGenerationSession {
            override fun cancel() {
                if (!terminal.compareAndSet(false, true)) return
                runCatching { conversation.cancelProcess() }
                finishConversation(conversation)
            }

            override fun close() {
                if (!terminal.compareAndSet(false, true)) return
                finishConversation(conversation)
            }
        }

        try {
            val callback = object : MessageCallback {
                override fun onMessage(message: Message) {
                    if (!terminal.get()) listener.onChunk(message.toString())
                }

                override fun onDone() {
                    if (!terminal.compareAndSet(false, true)) return
                    finishConversation(conversation)
                    listener.onDone()
                }

                override fun onError(throwable: Throwable) {
                    if (!terminal.compareAndSet(false, true)) return
                    finishConversation(conversation)
                    listener.onError(throwable)
                }
            }
            val image = request.imagePng
            if (image == null) {
                conversation.sendMessageAsync(
                    request.prompt,
                    callback,
                    maxOutputToken = request.maxOutputTokens,
                )
            } else {
                conversation.sendMessageAsync(
                    Contents.of(
                        Content.Text(request.prompt),
                        Content.ImageBytes(image),
                    ),
                    callback,
                    maxOutputToken = request.maxOutputTokens,
                )
            }
        } catch (error: Throwable) {
            if (terminal.compareAndSet(false, true)) finishConversation(conversation)
            throw error
        }
        return session
    }

    override fun unload() {
        activeConversation?.let { conversation ->
            runCatching { conversation.cancelProcess() }
            finishConversation(conversation)
        }
        val current = engine
        engine = null
        if (current != null && current.isInitialized()) current.close()
    }

    override fun close() {
        unload()
    }

    private fun finishConversation(conversation: Conversation) {
        synchronized(this) {
            if (activeConversation === conversation) activeConversation = null
        }
        runCatching { if (conversation.isAlive) conversation.close() }
    }

    private fun LocalModelBackend.toLiteRtBackend(): Backend = when (this) {
        LocalModelBackend.CPU -> Backend.CPU()
        LocalModelBackend.GPU -> Backend.GPU()
        LocalModelBackend.NPU -> Backend.NPU(appContext.applicationInfo.nativeLibraryDir)
    }
}
