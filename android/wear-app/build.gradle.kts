plugins {
    alias(libs.plugins.android.application)
    alias(libs.plugins.kotlin.compose)
}

android {
    namespace = "ai.zara.wear"
    compileSdk = 37

    defaultConfig {
        applicationId = "ai.zara.wear"
        minSdk = 30
        targetSdk = 36
        versionCode = 1
        versionName = "0.1.0-alpha"
    }

    buildFeatures {
        compose = true
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
    testImplementation(libs.junit)
}
