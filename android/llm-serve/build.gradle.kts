import java.util.Properties

plugins {
    alias(libs.plugins.android.application)
}

fun loadZaraVersionProperties(projectRoot: java.io.File): Properties {
    val versionFile = projectRoot.resolve("../version.properties")
    require(versionFile.isFile) {
        "Canonical Zara version context is missing: " + versionFile.absolutePath
    }
    return Properties().apply {
        versionFile.inputStream().use { load(it) }
    }
}

val zaraVersionProperties = loadZaraVersionProperties(rootProject.projectDir)
val zaraVersionName = requireNotNull(zaraVersionProperties.getProperty("zara.version")) {
    "version.properties is missing zara.version"
}
val zaraAndroidVersionCode =
    zaraVersionProperties.getProperty("android.versionCode")?.toIntOrNull()
        ?: error("version.properties android.versionCode must be an integer")

val debugSigningKeystore = providers.environmentVariable("ZARA_ANDROID_DEBUG_KEYSTORE").orNull

android {
    namespace = "ai.zara.llmserve"
    compileSdk = 37

    defaultConfig {
        applicationId = "ai.zara.llmserve"
        minSdk = 29
        targetSdk = 36
        versionCode = zaraAndroidVersionCode
        versionName = zaraVersionName
    }

    sourceSets {
        getByName("main") {
            kotlin.directories += "../app/src/main/java/ai/zara/app/localai"
        }
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
    implementation(libs.litert.lm.android)
    testImplementation(libs.junit)
    testImplementation(libs.org.json)
}
