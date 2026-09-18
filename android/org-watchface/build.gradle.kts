plugins {
    alias(libs.plugins.android.application)
}

android {
    namespace = "ai.zara.watchface.orgtime"
    compileSdk = 37

    defaultConfig {
        applicationId = "ai.zara.watchface.orgtime"
        minSdk = 33
        targetSdk = 36
        versionCode = 1
        versionName = "0.1.0-alpha"
    }

    buildTypes {
        release {
            isMinifyEnabled = false
        }
    }
}
