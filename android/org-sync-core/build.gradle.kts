plugins {
    alias(libs.plugins.android.library)
}

android {
    namespace = "ai.zara.org.sync.core"
    compileSdk = 37

    defaultConfig {
        minSdk = 29
    }

    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_17
        targetCompatibility = JavaVersion.VERSION_17
    }
}

dependencies {
    implementation(libs.jgit)
    testImplementation(libs.junit)
}
