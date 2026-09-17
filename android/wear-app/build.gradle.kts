plugins {
    alias(libs.plugins.android.application)
    alias(libs.plugins.kotlin.compose)
}

val debugSigningKeystore = providers.environmentVariable("ZARA_ANDROID_DEBUG_KEYSTORE").orNull

android {
    namespace = "ai.zara.wear"
    compileSdk = 37

    defaultConfig {
        applicationId = "ai.zara.app"
        minSdk = 30
        targetSdk = 36
        versionCode = 3
        versionName = "0.1.2-alpha"
    }

    buildFeatures {
        compose = true
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
    implementation(project(":shared-ui"))
    implementation(platform(libs.compose.bom))
    implementation(libs.activity.compose)
    implementation(libs.compose.ui)
    implementation(libs.compose.ui.tooling.preview)
    implementation(libs.wear.compose.foundation)
    implementation(libs.wear.compose.material3)
    implementation(libs.play.services.wearable)
    implementation(libs.wear.tiles)
    implementation(libs.wear.protolayout)
    implementation(libs.wear.protolayout.material)
    implementation(libs.wear.protolayout.material3)
    implementation(libs.wear.watchface.complications.data.source.ktx)
    testImplementation(libs.wear.tiles.testing)
    testImplementation(libs.junit)
}
