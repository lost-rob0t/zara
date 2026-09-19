pluginManagement {
    repositories {
        google()
        mavenCentral()
        gradlePluginPortal()
    }
}

dependencyResolutionManagement {
    repositoriesMode.set(RepositoriesMode.FAIL_ON_PROJECT_REPOS)
    repositories {
        google()
        mavenCentral()
    }
}

rootProject.name = "zara-android"
include(":wear-voice")
include(":app")
include(":shared-ui")
include(":editor-core")
include(":code-editor")
include(":org-core")
include(":org-storage")
include(":org-app")
include(":org-sync-core")
include(":org-notebook")
