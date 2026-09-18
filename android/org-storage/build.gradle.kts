plugins {
    alias(libs.plugins.android.library)
}

android {
    namespace = "ai.zara.org.storage"
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
    implementation(project(":org-core"))
    implementation(libs.documentfile)
    testImplementation(libs.junit)
}
