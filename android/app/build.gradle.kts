import org.gradle.api.DefaultTask
import org.gradle.api.file.DirectoryProperty
import org.gradle.api.tasks.InputDirectory
import org.gradle.api.tasks.OutputDirectory
import org.gradle.api.tasks.TaskAction

plugins {
    alias(libs.plugins.android.application)
}

abstract class GeneratePortableSemanticAssets : DefaultTask() {
    @get:InputDirectory
    abstract val repositoryRoot: DirectoryProperty

    @get:OutputDirectory
    abstract val outputDirectory: DirectoryProperty

    @TaskAction
    fun generate() {
        val root = repositoryRoot.get().asFile
        val output = outputDirectory.get().asFile
        output.deleteRecursively()
        project.copy {
            into(output)
            from(root.resolve("modules/intent_frames.pl")) {
                into("prolog/shared/modules")
            }
            from(root.resolve("modules/normalizer.pl")) {
                into("prolog/shared/modules")
            }
            from(root.resolve("kb/intents.pl")) {
                into("prolog/shared/kb")
            }
        }
    }
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

androidComponents {
    onVariants(selector().all()) { variant ->
        val taskName = "generate${variant.name.replaceFirstChar(Char::uppercaseChar)}PortableSemanticAssets"
        val generateAssets = tasks.register<GeneratePortableSemanticAssets>(taskName) {
            repositoryRoot.set(layout.projectDirectory.dir("../.."))
        }
        variant.sources.assets?.addGeneratedSourceDirectory(
            generateAssets,
            GeneratePortableSemanticAssets::outputDirectory
        )
    }
}

dependencies {
    testImplementation(libs.junit)
}
