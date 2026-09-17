plugins {
    alias(libs.plugins.android.application)
}

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
