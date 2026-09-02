plugins {
    alias(libs.plugins.android.application)
}

val androidNdkVersion = providers.environmentVariable("ZARA_ANDROID_NDK_VERSION").orNull
    ?: error("ZARA_ANDROID_NDK_VERSION must be supplied by the pinned Android Nix toolchain")
val treallaSourceDir = providers.environmentVariable("ZARA_TREALLA_SOURCE_DIR").orNull ?: ""
val treallaLibraryRoot = providers.environmentVariable("ZARA_TREALLA_LIBRARY_ROOT").orNull ?: ""

android {
    namespace = "ai.zara.app"
    compileSdk = 37
    ndkVersion = androidNdkVersion

    defaultConfig {
        applicationId = "ai.zara.app"
        minSdk = 29
        targetSdk = 36
        versionCode = 1
        versionName = "0.1.0-skeleton"

        ndk {
            abiFilters += setOf("arm64-v8a", "x86_64")
        }

        externalNativeBuild {
            cmake {
                arguments += listOf(
                    "-DZARA_TREALLA_SOURCE_DIR=$treallaSourceDir",
                    "-DZARA_TREALLA_LIBRARY_ROOT=$treallaLibraryRoot"
                )
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

    externalNativeBuild {
        cmake {
            path = file("src/main/cpp/CMakeLists.txt")
            version = "3.22.1"
        }
    }
}

dependencies {
    testImplementation(libs.junit)
}
