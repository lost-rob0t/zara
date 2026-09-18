import java.util.Properties

plugins {
    alias(libs.plugins.android.application)
    alias(libs.plugins.kotlin.compose)
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
require(zaraVersionName.matches(Regex(
    """^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)(-[0-9A-Za-z.-]+)?(\+[0-9A-Za-z.-]+)?$"""
))) {
    "version.properties zara.version must be SemVer"
}
require(zaraAndroidVersionCode in 1..2100000000) {
    "version.properties android.versionCode is outside Android's valid range"
}

android {
    namespace = "ai.zara.wear.voice"
    compileSdk = 37

    defaultConfig {
        applicationId = "ai.zara.wear.voice"
        minSdk = 30
        targetSdk = 36
        versionCode = zaraAndroidVersionCode
        versionName = zaraVersionName
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
    implementation(libs.wear.compose.foundation)
    implementation(libs.wear.compose.material3)
    testImplementation(libs.junit)
}
