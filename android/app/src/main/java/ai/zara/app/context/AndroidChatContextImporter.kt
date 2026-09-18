package ai.zara.app.context

import android.content.ContentResolver
import android.net.Uri
import android.provider.OpenableColumns
import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets

object AndroidChatContextImporter {
    fun importAll(
        resolver: ContentResolver,
        uris: List<Uri>,
    ): List<PendingChatContextAttachment> = uris.map { uri ->
        importOne(resolver, uri)
    }

    fun importOne(
        resolver: ContentResolver,
        uri: Uri,
    ): PendingChatContextAttachment {
        val name = displayName(resolver, uri)
        val mimeType = resolver.getType(uri)?.trim()?.lowercase()
            ?.takeIf { it.isNotEmpty() }
            ?: "application/octet-stream"

        require(isSupportedText(name, mimeType)) {
            "Unsupported context file '$name'. This slice accepts text/code/data files; binary/PDF/image extraction must use a typed extractor."
        }

        val bytes = resolver.openInputStream(uri)?.use(::readBounded)
            ?: throw IllegalArgumentException("Could not open context file '$name'")
        require(bytes.isNotEmpty()) { "Context file '$name' is empty" }

        val decoder = StandardCharsets.UTF_8.newDecoder()
            .onMalformedInput(CodingErrorAction.REPORT)
            .onUnmappableCharacter(CodingErrorAction.REPORT)
        val text = try {
            decoder.decode(ByteBuffer.wrap(bytes)).toString()
        } catch (_: Exception) {
            throw IllegalArgumentException("Context file '$name' is not valid UTF-8 text")
        }

        return PendingChatContextAttachment(
            name = name,
            mimeType = mimeType,
            text = text,
        )
    }

    private fun displayName(resolver: ContentResolver, uri: Uri): String {
        val fromProvider = resolver.query(
            uri,
            arrayOf(OpenableColumns.DISPLAY_NAME),
            null,
            null,
            null,
        )?.use { cursor ->
            if (!cursor.moveToFirst()) return@use null
            val index = cursor.getColumnIndex(OpenableColumns.DISPLAY_NAME)
            if (index < 0) null else cursor.getString(index)
        }
        return fromProvider?.trim()?.takeIf { it.isNotEmpty() }
            ?: uri.lastPathSegment?.substringAfterLast('/')?.trim()?.takeIf { it.isNotEmpty() }
            ?: "context.txt"
    }

    private fun readBounded(input: java.io.InputStream): ByteArray {
        val max = ChatContextLimits.MAX_ATTACHMENT_BYTES
        val output = ByteArrayOutputStream(minOf(max, 16 * 1024))
        val buffer = ByteArray(8 * 1024)
        var total = 0
        while (true) {
            val read = input.read(buffer)
            if (read < 0) break
            total += read
            require(total <= max) {
                "Context attachment exceeds $max bytes"
            }
            output.write(buffer, 0, read)
        }
        return output.toByteArray()
    }

    private fun isSupportedText(name: String, mimeType: String): Boolean {
        if (mimeType.startsWith("text/")) return true
        if (mimeType in TEXT_APPLICATION_MIME_TYPES) return true
        val extension = name.substringAfterLast('.', missingDelimiterValue = "").lowercase()
        return extension in TEXT_EXTENSIONS
    }

    private val TEXT_APPLICATION_MIME_TYPES = setOf(
        "application/json",
        "application/ld+json",
        "application/xml",
        "application/javascript",
        "application/x-javascript",
        "application/yaml",
        "application/x-yaml",
        "application/toml",
        "application/sql",
        "application/x-sh",
        "application/x-shellscript",
    )

    private val TEXT_EXTENSIONS = setOf(
        "txt", "md", "markdown", "org", "rst", "csv", "tsv",
        "json", "jsonl", "yaml", "yml", "toml", "ini", "conf",
        "xml", "html", "htm", "css", "scss",
        "js", "jsx", "ts", "tsx",
        "py", "pyi", "rb", "php", "go", "rs", "nim",
        "c", "h", "cc", "cpp", "cxx", "hpp",
        "java", "kt", "kts", "gradle",
        "lisp", "cl", "el", "scm", "rkt",
        "pl", "prolog", "sql", "sh", "bash", "zsh", "fish",
        "tex", "bib", "log",
    )
}
