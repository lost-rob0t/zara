pluginManagement {
    repositories {
        google {
            content {
                includeGroupByRegex("com\\.android.*")
                includeGroupByRegex("com\\.google.*")
                includeGroupByRegex("androidx.*")
            }
        }
        mavenCentral()
        gradlePluginPortal()
    }
}

dependencyResolutionManagement {
    repositoriesMode.set(RepositoriesMode.FAIL_ON_PROJECT_REPOS)
    repositories {
        google()
        mavenCentral()
        maven { url = uri("https://jitpack.io") }
    }
}

rootProject.name = "zara-android"
include(":app")
include(":shared-ui")
include(":editor-core")
include(":code-editor")
include(":termux-bridge")
include(":org-core")
include(":org-storage")
include(":org-sync-core")
include(":org-surfaces")
include(":org-app")
include(":org-editor")
include(":org-todo")
include(":org-reminder")
include(":org-timer")
include(":org-roam")
include(":org-graph")
include(":org-home")
include(":wear-app")
include(":wear-voice")
