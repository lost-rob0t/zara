plugins {
    alias(libs.plugins.android.application)
}

val debugSigningKeystore = providers.environmentVariable("ZARA_ANDROID_DEBUG_KEYSTORE").orNull

android {
    enableKotlin = false
    namespace = "ai.zara.agenda"
    compileSdk = 37

    defaultConfig {
        applicationId = "ai.zara.agenda"
        minSdk = 33
        targetSdk = 36
        versionCode = 1
        versionName = "0.1.0-alpha"
    }

    signingConfigs {
        getByName("debug") {
            if (debugSigningKeystore != null) {
                val keyFile = file(debugSigningKeystore)
                require(keyFile.isFile) { "Zara Android debug signing keystore is missing" }
                storeFile = keyFile
                storePassword = "android"
                keyAlias = "androiddebugkey"
                keyPassword = "android"
            }
        }
    }

    buildTypes {
        debug {
            isMinifyEnabled = true
        }
        release {
            isMinifyEnabled = true
            isShrinkResources = false
        }
    }
}
