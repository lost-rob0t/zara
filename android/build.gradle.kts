plugins {
    alias(libs.plugins.android.application) apply false
    alias(libs.plugins.android.library) apply false
    alias(libs.plugins.kotlin.compose) apply false
}

project(":app") {
    pluginManager.withPlugin("com.android.application") {
        extensions.configure<com.android.build.api.dsl.ApplicationExtension> {
            defaultConfig {
                testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"
            }
        }
        dependencies.add("androidTestImplementation", "junit:junit:4.13.2")
        dependencies.add("androidTestImplementation", "androidx.test:runner:1.7.0")
    }
}
