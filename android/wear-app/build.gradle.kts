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

val debugSigningKeystore = providers.environmentVariable("ZARA_ANDROID_DEBUG_KEYSTORE").orNull
val samsungHealthSensorAars = fileTree("libs") {
    include("samsung-health-sensor-api-*.aar")
}
val samsungHealthSensorAarFiles = samsungHealthSensorAars.files
require(samsungHealthSensorAarFiles.size <= 1) {
    "Keep exactly one Samsung Health Sensor SDK AAR under android/wear-app/libs"
}
val hasSamsungHealthSensorSdk = samsungHealthSensorAarFiles.size == 1

android {
    namespace = "ai.zara.wear"
    compileSdk = 37

    defaultConfig {
        applicationId = "ai.zara.app"
        // Compatibility floor: Galaxy Watch5 Pro-class Wear OS hardware and newer.
        minSdk = 30
        targetSdk = 36
        versionCode = zaraAndroidVersionCode
        versionName = zaraVersionName
        buildConfigField("boolean", "HAS_SAMSUNG_HEALTH_SENSOR_SDK", hasSamsungHealthSensorSdk.toString())
    }

    buildFeatures {
        buildConfig = true
        compose = true
    }

    if (hasSamsungHealthSensorSdk) {
        sourceSets.getByName("main").java.srcDir("src/samsungHealthSensorSdk/java")
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
    implementation(project(":shared-ui"))
    implementation(platform(libs.compose.bom))
    implementation(libs.activity.compose)
    implementation(libs.compose.ui)
    implementation(libs.compose.ui.tooling.preview)
    implementation(libs.wear.compose.foundation)
    implementation(libs.wear.compose.material3)
    implementation(libs.wear.watchface.complications.data.source.ktx)
    implementation(libs.play.services.wearable)
    if (hasSamsungHealthSensorSdk) {
        implementation(samsungHealthSensorAars)
    }
    testImplementation(libs.junit)
}
