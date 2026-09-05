package ai.zara.app.auth

import android.content.Context
import java.io.File

sealed interface EnrollmentState {
    data object Unenrolled : EnrollmentState
    data class AwaitingServerPin(val publicKey: ByteArray) : EnrollmentState
    data class Ready(val publicKey: ByteArray) : EnrollmentState
    data class Corrupt(val reason: String) : EnrollmentState
}

class EnrollmentRepository(
    private val credentials: WrappedCredentialStore,
    private val serverPins: ServerPinStore,
    private val generator: CurveCredentialGenerator,
    private val configurator: CurveAuthConfigurator = CurveAuthConfigurator(),
) {
    fun state(): EnrollmentState {
        return when (val credentialResult = credentials.load()) {
            CredentialLoadResult.Unenrolled -> EnrollmentState.Unenrolled
            is CredentialLoadResult.Corrupt -> EnrollmentState.Corrupt(credentialResult.reason)
            is CredentialLoadResult.Ready -> {
                val credential = credentialResult.credential
                try {
                    when (val pinResult = serverPins.load()) {
                        ServerPinLoadResult.Missing ->
                            EnrollmentState.AwaitingServerPin(credential.publicKey)
                        is ServerPinLoadResult.Corrupt -> EnrollmentState.Corrupt(pinResult.reason)
                        is ServerPinLoadResult.Ready -> EnrollmentState.Ready(credential.publicKey)
                    }
                } finally {
                    credential.destroy()
                }
            }
        }
    }

    fun createIdentity(): ByteArray {
        when (val existing = credentials.load()) {
            is CredentialLoadResult.Corrupt ->
                throw AuthenticationException("stored CURVE credential is corrupt: ${existing.reason}")
            is CredentialLoadResult.Ready -> {
                existing.credential.destroy()
                throw AuthenticationException("CURVE identity already exists")
            }
            CredentialLoadResult.Unenrolled -> Unit
        }
        val credential = generator.generate()
        return try {
            credentials.save(credential)
            credential.publicKey
        } finally {
            credential.destroy()
        }
    }

    fun pinServer(publicKey: ByteArray) {
        serverPins.save(ServerPin(publicKey))
    }

    fun configure(socket: CurveSocketOptions) {
        val credential = when (val loaded = credentials.load()) {
            CredentialLoadResult.Unenrolled ->
                throw AuthenticationException("Android client is not enrolled")
            is CredentialLoadResult.Corrupt ->
                throw AuthenticationException("stored CURVE credential is corrupt: ${loaded.reason}")
            is CredentialLoadResult.Ready -> loaded.credential
        }
        try {
            val pin = when (val loaded = serverPins.load()) {
                ServerPinLoadResult.Missing ->
                    throw AuthenticationException("server CURVE public key is not pinned")
                is ServerPinLoadResult.Corrupt ->
                    throw AuthenticationException("stored server pin is corrupt: ${loaded.reason}")
                is ServerPinLoadResult.Ready -> loaded.pin
            }
            configurator.configure(socket, credential, pin)
        } finally {
            credential.destroy()
        }
    }

    fun resetEnrollment(deleteWrappingKey: (() -> Boolean)? = null): Boolean {
        val credentialsDeleted = credentials.delete()
        val pinDeleted = serverPins.delete()
        val wrappingKeyDeleted = deleteWrappingKey?.invoke() ?: true
        return credentialsDeleted && pinDeleted && wrappingKeyDeleted
    }
}

object AndroidEnrollmentRepository {
    fun create(context: Context): EnrollmentRepository {
        val authDirectory = File(context.noBackupFilesDir, "zara/auth")
        val cipher = AndroidKeystoreCredentialCipher()
        return EnrollmentRepository(
            credentials = WrappedCredentialStore(
                File(authDirectory, "curve-credential.bin"),
                cipher,
            ),
            serverPins = ServerPinStore(File(authDirectory, "server-pin.bin")),
            generator = JeroMqCurveCredentialGenerator(),
        )
    }
}
