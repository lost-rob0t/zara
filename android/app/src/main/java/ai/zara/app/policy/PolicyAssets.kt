package ai.zara.app.policy

import java.io.File
import java.nio.file.Files
import java.nio.file.StandardCopyOption
import java.nio.file.AtomicMoveNotSupportedException

object PolicyAssets {
    const val CONFIG_NAME = "policy-config.pl"
    val defaultConfig = """
        % Trusted executable configuration. Model output is never consulted.
        :- multifile zara_policy:option/2.
        :- multifile zara_policy:user_rule/6.
        :- multifile zara_policy:disabled/1.
        :- multifile zara_policy:suppress/2.
        zara_policy:option(mode, advice).
        zara_policy:option(max_findings, 8).
        zara_policy:option(disabled_categories, [style]).
    """.trimIndent() + "\n"

    fun stage(directory: File, read: (String) -> ByteArray): File {
        check(directory.isDirectory || directory.mkdirs()) { "Policy asset directory is unavailable" }
        val assets = listOf("defaults.pl", "policy.pl").associateWith(read)
        require(assets.values.all { it.size in 1..65_536 }) { "Invalid packaged policy asset size" }
        assets.forEach { (name, bytes) -> atomicWrite(File(directory, name), bytes) }
        return File(directory, "policy.pl")
    }

    fun seedConfig(file: File) {
        check(file.parentFile.isDirectory || file.parentFile.mkdirs()) { "Policy config directory is unavailable" }
        if (!file.exists()) {
            try {
                Files.write(file.toPath(), defaultConfig.toByteArray(Charsets.UTF_8),
                    java.nio.file.StandardOpenOption.CREATE_NEW, java.nio.file.StandardOpenOption.WRITE)
            } catch (_: java.nio.file.FileAlreadyExistsException) {
                check(file.isFile) { "Policy configuration is not a regular file" }
            }
        }
        check(file.isFile) { "Policy configuration is not a regular file" }
    }

    private fun atomicWrite(target: File, bytes: ByteArray) {
        val temporary = File.createTempFile(".policy-", ".tmp", target.parentFile)
        try {
            temporary.writeBytes(bytes)
            try {
                Files.move(temporary.toPath(), target.toPath(), StandardCopyOption.REPLACE_EXISTING,
                    StandardCopyOption.ATOMIC_MOVE)
            } catch (_: AtomicMoveNotSupportedException) {
                Files.move(temporary.toPath(), target.toPath(), StandardCopyOption.REPLACE_EXISTING)
            }
        } finally { temporary.delete() }
    }
}
