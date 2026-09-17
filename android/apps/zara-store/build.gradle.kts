plugins {
    alias(libs.plugins.android.application)
    alias(libs.plugins.kotlin.compose)
}

val sourceSha = providers.environmentVariable("ZARA_SOURCE_SHA").orNull
    ?: providers.exec {
        commandLine("git", "rev-parse", "HEAD")
    }.standardOutput.asText.get().trim()
require(sourceSha.matches(Regex("[0-9a-f]{40}"))) {
    "Zara Store source SHA must be an immutable 40-character lowercase git SHA"
}

android {
    namespace = "ai.zara.store"
    compileSdk = 37

    defaultConfig {
        applicationId = "ai.zara.store"
        minSdk = 29
        targetSdk = 36
        versionCode = 1
        versionName = "0.1.0-alpha"
        buildConfigField("String", "SOURCE_SHA", "\"$sourceSha\"")
    }

    buildFeatures {
        buildConfig = true
        compose = true
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
    implementation(libs.compose.material3)

    // Apache-2.0 F-Droid client foundations. Keep all three on one pinned release.
    implementation(libs.fdroid.download)
    implementation(libs.fdroid.index)
    implementation(libs.fdroid.database)

    testImplementation(libs.junit)
}
