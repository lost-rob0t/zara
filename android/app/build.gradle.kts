plugins {
    alias(libs.plugins.android.application)
}

android {
    namespace = "ai.zara.app"
    compileSdk = 37

    defaultConfig {
        applicationId = "ai.zara.app"
        minSdk = 29
        targetSdk = 36
        versionCode = 1
        versionName = "0.1.0-skeleton"
    }

    buildTypes {
        release {
            isMinifyEnabled = false
        }
    }

    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_17
        targetCompatibility = JavaVersion.VERSION_17
    }
}

dependencies {
    testImplementation(libs.junit)
}
