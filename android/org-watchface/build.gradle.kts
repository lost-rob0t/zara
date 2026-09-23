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
require(zaraVersionName.matches(Regex(
    """^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)(-[0-9A-Za-z.-]+)?(\+[0-9A-Za-z.-]+)?$"""
))) {
    "version.properties zara.version must be SemVer"
}
require(zaraAndroidVersionCode in 1..2100000000) {
    "version.properties android.versionCode is outside Android's valid range"
}

android {
    namespace = "ai.zara.watchface.orgtime"
    compileSdk = 37

    defaultConfig {
        applicationId = "ai.zara.watchface.orgtime"
        minSdk = 33
        targetSdk = 36
        versionCode = zaraAndroidVersionCode
        versionName = zaraVersionName
    }

    buildTypes {
        release {
            isMinifyEnabled = false
        }
    }
}
