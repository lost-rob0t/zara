plugins {
    alias(libs.plugins.android.application) apply false
    alias(libs.plugins.android.library) apply false
    alias(libs.plugins.kotlin.compose) apply false
}

project(":app") {
    pluginManager.withPlugin("com.android.application") {
        dependencies.add("androidTestImplementation", "junit:junit:4.13.2")
        dependencies.add("androidTestImplementation", "androidx.test:runner:1.7.0")
    }
}
