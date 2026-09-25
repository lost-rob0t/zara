package ai.zara.app.update

interface UpdateApkProvenance {
    val release: UpdateRelease
    val versionName: String
    val versionCode: Long
}
