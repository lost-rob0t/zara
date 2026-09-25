import groovy.json.JsonSlurper
import java.util.Properties
import org.gradle.api.DefaultTask
import org.gradle.api.file.ConfigurableFileCollection
import org.gradle.api.file.DirectoryProperty
import org.gradle.api.file.RegularFileProperty
import org.gradle.api.tasks.InputFile
import org.gradle.api.tasks.InputFiles
import org.gradle.api.tasks.OutputDirectory
import org.gradle.api.tasks.PathSensitive
import org.gradle.api.tasks.PathSensitivity
import org.gradle.api.tasks.TaskAction

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
        val symbolicDialogue = checkNotNull(sources["symbolic_dialogue.pl"]) {
            "symbolic_dialogue.pl input is required"
        }
        val symbolicDialogueTurn = checkNotNull(sources["symbolic_dialogue_turn.pl"]) {
            "symbolic_dialogue_turn.pl input is required"
        }
        val intents = checkNotNull(sources["intents.pl"]) { "intents.pl input is required" }
        val changelog = checkNotNull(sources["CHANGELOG.md"]) { "CHANGELOG.md input is required" }
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
            from(symbolicDialogue) {
                into("prolog/shared/modules")
            }
            from(symbolicDialogueTurn) {
                into("prolog/shared/modules")
            }
            from(intents) {
                into("prolog/shared/kb")
            }
            from(changelog)
        }
    }
}

abstract class GeneratePortableConversationSchema : DefaultTask() {
    @get:InputFile
    @get:PathSensitive(PathSensitivity.RELATIVE)
    abstract val sourceFile: RegularFileProperty

    @get:OutputDirectory
    abstract val outputDirectory: DirectoryProperty

    @TaskAction
    fun generate() {
        val source = sourceFile.get().asFile
        require(source.isFile) { "canonical conversation_schema.sql is required" }
        val output = outputDirectory.get().asFile
        output.deleteRecursively()
        project.copy {
            from(source)
            into(output.resolve("database"))
            rename { "conversation_schema.sql" }
        }
    }
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

val samsungHealthAars = fileTree("libs") {
    include("samsung-health-data-api-*.aar")
}
val samsungHealthAarFiles = samsungHealthAars.files
require(samsungHealthAarFiles.size <= 1) {
    "Keep exactly one Samsung Health Data SDK AAR under android/app/libs"
}
val hasSamsungHealthSdk = samsungHealthAarFiles.size == 1

val androidNdkVersion = providers.environmentVariable("ZARA_ANDROID_NDK_VERSION").orNull
    ?: error("ZARA_ANDROID_NDK_VERSION must be supplied by the pinned Android Nix toolchain")
val treallaSourceDir = providers.environmentVariable("ZARA_TREALLA_SOURCE_DIR").orNull ?: ""
val treallaLibraryRoot = providers.environmentVariable("ZARA_TREALLA_LIBRARY_ROOT").orNull ?: ""
val debugSigningKeystore = providers.environmentVariable("ZARA_ANDROID_DEBUG_KEYSTORE").orNull
val sourceSha = providers.environmentVariable("ZARA_SOURCE_SHA").orNull
    ?: githubPullRequestHeadSha()
    ?: providers.exec {
        commandLine("git", "rev-parse", "HEAD")
    }.standardOutput.asText.get().trim()
require(sourceSha.matches(Regex("[0-9a-f]{40}"))) {
    "Zara source SHA must be an immutable 40-character lowercase git SHA"
}

android {
    namespace = "ai.zara.app"
    compileSdk = 37
    ndkVersion = androidNdkVersion

    defaultConfig {
        applicationId = "ai.zara.app"
        minSdk = 29
        targetSdk = 36
        versionCode = zaraAndroidVersionCode
        versionName = zaraVersionName
        buildConfigField("String", "SOURCE_SHA", "\"$sourceSha\"")
        buildConfigField("boolean", "HAS_SAMSUNG_HEALTH_SDK", hasSamsungHealthSdk.toString())

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
        compose = true
    }

    if (hasSamsungHealthSdk) {
        sourceSets.getByName("main").java.srcDir("src/samsungHealthSdk/java")
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
                layout.projectDirectory.file("../../modules/symbolic_dialogue.pl"),
                layout.projectDirectory.file("../../modules/symbolic_dialogue_turn.pl"),
                layout.projectDirectory.file("../../kb/intents.pl"),
                layout.projectDirectory.file("../../CHANGELOG.md")
            )
            outputDirectory.convention(
                layout.buildDirectory.dir("generated/portableSemanticAssets/${variant.name}")
            )
        }
        variant.sources.assets?.addGeneratedSourceDirectory(
            generateAssets,
            GeneratePortableSemanticAssets::outputDirectory
        )

        val schemaTaskName = "generate${variant.name.replaceFirstChar(Char::uppercaseChar)}PortableConversationSchema"
        val generateSchema = tasks.register<GeneratePortableConversationSchema>(schemaTaskName) {
            sourceFile.set(layout.projectDirectory.file("../../zara/conversation_schema.sql"))
            outputDirectory.convention(
                layout.buildDirectory.dir("generated/portableConversationSchema/${variant.name}")
            )
        }
        variant.sources.assets?.addGeneratedSourceDirectory(
            generateSchema,
            GeneratePortableConversationSchema::outputDirectory
        )
    }
}

dependencies {
    implementation(project(":shared-ui"))
    implementation(platform(libs.compose.bom))
    implementation(libs.activity.compose)
    implementation(libs.compose.ui)
    implementation(libs.compose.material3)
    implementation(libs.compose.ui.tooling.preview)
    implementation(libs.jeromq)
    implementation(libs.play.services.wearable)
    implementation(libs.play.services.code.scanner)
    implementation(libs.libadb.android)
    implementation(libs.conscrypt.android)
    implementation(libs.bcpkix)
    implementation(libs.jgit)
    implementation(libs.litert.lm.android)
    if (hasSamsungHealthSdk) {
        implementation(libs.gson)
        implementation(samsungHealthAars)
    }
    testImplementation(libs.junit)
    testImplementation(libs.org.json)
}
