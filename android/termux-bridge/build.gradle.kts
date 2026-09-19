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

android {
    namespace = "ai.zara.termux.bridge"
    compileSdk = 37

    defaultConfig {
        applicationId = "ai.zara.termux.bridge"
        minSdk = 29
        targetSdk = 36
        versionCode = zaraAndroidVersionCode
        versionName = zaraVersionName
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
