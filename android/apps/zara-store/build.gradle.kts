import groovy.json.JsonSlurper

plugins {
    alias(libs.plugins.android.application)
    alias(libs.plugins.kotlin.compose)
}

fun githubPullRequestHeadSha(): String? {
    val eventPath = providers.environmentVariable("GITHUB_EVENT_PATH").orNull ?: return null
    val eventFile = file(eventPath)
    if (!eventFile.isFile) return null
    val payload = JsonSlurper().parse(eventFile) as? Map<*, *> ?: return null
    val pullRequest = payload["pull_request"] as? Map<*, *> ?: return null
    val head = pullRequest["head"] as? Map<*, *> ?: return null
    return head["sha"] as? String
}

val debugSigningKeystore = providers.environmentVariable("ZARA_ANDROID_DEBUG_KEYSTORE").orNull
val sourceSha = providers.environmentVariable("ZARA_SOURCE_SHA").orNull
    ?: githubPullRequestHeadSha()
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
        versionName = "0.2.0-alpha"
        buildConfigField("String", "SOURCE_SHA", "\"$sourceSha\"")
    }

    buildFeatures {
        buildConfig = true
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
    implementation(project(":shared-ui"))
    implementation(platform(libs.compose.bom))
    implementation(libs.activity.compose)
    implementation(libs.compose.ui)
    implementation(libs.compose.material3)
    implementation(libs.gson)

    // Apache-2.0 F-Droid client foundations. Keep all three on one pinned release.
    implementation(libs.fdroid.download)
    implementation(libs.fdroid.index)
    implementation(libs.fdroid.database)

    testImplementation(libs.junit)
}
