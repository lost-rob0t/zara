package ai.zara.app.watch

import android.content.Context
import android.os.Build
import io.github.muntashirakon.adb.AbsAdbConnectionManager
import java.security.PrivateKey
import java.security.cert.Certificate
import java.util.concurrent.TimeUnit

class WatchAdbConnection private constructor(
    private val identity: WatchAdbIdentity.Identity,
) : AbsAdbConnectionManager() {
    init {
        setApi(Build.VERSION.SDK_INT)
        setTimeout(20, TimeUnit.SECONDS)
    }

    override fun getPrivateKey(): PrivateKey = identity.privateKey

    override fun getCertificate(): Certificate = identity.certificate

    override fun getDeviceName(): String = "Zara (${Build.MODEL})"

    companion object {
        @Volatile
        private var instance: WatchAdbConnection? = null

        fun get(context: Context): WatchAdbConnection =
            instance ?: synchronized(this) {
                instance ?: WatchAdbConnection(
                    WatchAdbIdentity.loadOrCreate(context.applicationContext.filesDir)
                ).also { instance = it }
            }
    }
}
