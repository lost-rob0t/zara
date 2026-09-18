val debugSigningKeystore = providers.environmentVariable("ZARA_ANDROID_DEBUG_KEYSTORE").orNull

plugins {
    alias(libs.plugins.android.application)
    alias(libs.plugins.kotlin.compose)
}

android {
    namespace = "ai.zara.org.notebook"
    compileSdk = 37

    defaultConfig {
        applicationId = "ai.zara.org.notebook"
        minSdk = 29
        targetSdk = 36
        versionCode = 1
        versionName = "0.1.0-alpha"
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

    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_17
        targetCompatibility = JavaVersion.VERSION_17
    }
}

dependencies {
    implementation(project(":org-core"))
    implementation(project(":org-storage"))
    implementation(project(":shared-ui"))
    implementation(platform(libs.compose.bom))
    implementation(libs.activity.compose)
    implementation(libs.compose.ui)
    implementation(libs.compose.material3)
    implementation(libs.compose.ui.tooling.preview)
    testImplementation(libs.junit)
}
