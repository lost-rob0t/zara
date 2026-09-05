import org.gradle.api.DefaultTask
import org.gradle.api.file.ConfigurableFileCollection
import org.gradle.api.file.DirectoryProperty
import org.gradle.api.tasks.InputFiles
import org.gradle.api.tasks.OutputDirectory
import org.gradle.api.tasks.PathSensitive
import org.gradle.api.tasks.PathSensitivity
import org.gradle.api.tasks.TaskAction

plugins {
    alias(libs.plugins.android.application)
}

abstract class GeneratePortableSemanticAssets : DefaultTask() {
    @get:InputFiles
    @get:PathSensitive(PathSensitivity.RELATIVE)
    abstract val sourceFiles: ConfigurableFileCollection

    @get:OutputDirectory
    abstract val outputDirectory: DirectoryProperty

    @TaskAction
    fun generate() {
        val sources = sourceFiles.files.associateBy { it.name }
        val intentFrames = checkNotNull(sources["intent_frames.pl"]) { "intent_frames.pl input is required" }
        val normalizer = checkNotNull(sources["normalizer.pl"]) { "normalizer.pl input is required" }
        val intents = checkNotNull(sources["intents.pl"]) { "intents.pl input is required" }
        val output = outputDirectory.get().asFile
        output.deleteRecursively()
        project.copy {
            into(output)
            from(intentFrames) {
                into("prolog/shared/modules")
            }
            from(normalizer) {
                into("prolog/shared/modules")
            }
            from(intents) {
                into("prolog/shared/kb")
            }
        }
    }
}

val androidNdkVersion = providers.environmentVariable("ZARA_ANDROID_NDK_VERSION").orNull
    ?: error("ZARA_ANDROID_NDK_VERSION must be supplied by the pinned Android Nix toolchain")
val treallaSourceDir = providers.environmentVariable("ZARA_TREALLA_SOURCE_DIR").orNull ?: ""
val treallaLibraryRoot = providers.environmentVariable("ZARA_TREALLA_LIBRARY_ROOT").orNull ?: ""
val sourceSha = providers.environmentVariable("ZARA_SOURCE_SHA")
    .orElse(providers.environmentVariable("GITHUB_SHA"))
    .orElse("development")
    .get()

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
        buildConfigField("String", "SOURCE_SHA", "\"$sourceSha\"")

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

    buildFeatures {
        buildConfig = true
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

androidComponents {
    onVariants(selector().all()) { variant ->
        val taskName = "generate${variant.name.replaceFirstChar(Char::uppercaseChar)}PortableSemanticAssets"
        val generateAssets = tasks.register<GeneratePortableSemanticAssets>(taskName) {
            sourceFiles.from(
                layout.projectDirectory.file("../../modules/intent_frames.pl"),
                layout.projectDirectory.file("../../modules/normalizer.pl"),
                layout.projectDirectory.file("../../kb/intents.pl")
            )
            outputDirectory.convention(
                layout.buildDirectory.dir("generated/portableSemanticAssets/${variant.name}")
            )
        }
        variant.sources.assets?.addGeneratedSourceDirectory(
            generateAssets,
            GeneratePortableSemanticAssets::outputDirectory
        )
    }
}

dependencies {
    implementation(libs.jeromq)
    testImplementation(libs.junit)
}
