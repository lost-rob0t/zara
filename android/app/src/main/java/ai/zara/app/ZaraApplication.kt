package ai.zara.app

import android.app.Application

class ZaraApplication : Application() {
    val appSession: AndroidAppSession by lazy(LazyThreadSafetyMode.SYNCHRONIZED) {
        AndroidAppSession(this)
    }
}
