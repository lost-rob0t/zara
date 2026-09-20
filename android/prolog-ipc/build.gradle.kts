plugins {
    alias(libs.plugins.android.library)
}

android {
    namespace = "ai.zara.prolog.ipc"
    compileSdk = 37

    defaultConfig {
        minSdk = 29
    }

    buildFeatures {
        aidl = true
    }

    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_17
        targetCompatibility = JavaVersion.VERSION_17
    }
}

dependencies {
    testImplementation(libs.junit)
}
