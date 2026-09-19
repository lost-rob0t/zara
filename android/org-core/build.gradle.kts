plugins {
    alias(libs.plugins.kotlin.jvm)
}

dependencies {
    implementation(project(":editor-core"))
    testImplementation(libs.junit)
}
