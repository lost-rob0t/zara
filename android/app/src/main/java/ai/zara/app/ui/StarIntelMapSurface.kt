package ai.zara.app.ui

import android.webkit.WebResourceRequest
import android.webkit.WebSettings
import android.webkit.WebView
import android.webkit.WebViewClient
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.viewinterop.AndroidView
import java.net.URI

private const val MAP_CLIENT = "zara-android"

internal fun starIntelMapEmbedUrl(baseUrl: String): String {
    val base = validatedMapUri(baseUrl)
    val query = listOfNotNull(
        base.rawQuery?.takeIf { it.isNotBlank() },
        "embed=1",
        "client=$MAP_CLIENT",
    ).joinToString("&")
    return URI(
        base.scheme,
        null,
        base.host,
        base.port,
        base.rawPath.ifNullOrBlank("/"),
        query,
        base.rawFragment,
    ).toASCIIString()
}

internal fun isAllowedStarIntelMapNavigation(baseUrl: String, targetUrl: String): Boolean {
    val base = runCatching { validatedMapUri(baseUrl) }.getOrNull() ?: return false
    val target = runCatching { validatedMapUri(targetUrl) }.getOrNull() ?: return false
    return base.host.equals(target.host, ignoreCase = true) &&
        effectivePort(base) == effectivePort(target)
}

private fun validatedMapUri(value: String): URI {
    val uri = URI(value.trim())
    require(uri.scheme.equals("https", ignoreCase = true)) { "StarIntel map URL must use HTTPS" }
    require(!uri.host.isNullOrBlank()) { "StarIntel map URL must have a host" }
    require(uri.userInfo == null) { "StarIntel map URL must not contain user info" }
    return uri
}

private fun effectivePort(uri: URI): Int = if (uri.port == -1) 443 else uri.port

private fun String?.ifNullOrBlank(fallback: String): String =
    if (this.isNullOrBlank()) fallback else this

private class StarIntelMapWebViewClient(
    private val baseUrl: String,
) : WebViewClient() {
    override fun shouldOverrideUrlLoading(view: WebView, request: WebResourceRequest): Boolean =
        !isAllowedStarIntelMapNavigation(baseUrl, request.url.toString())
}

@Composable
internal fun StarIntelMapSurface(
    baseUrl: String,
    modifier: Modifier = Modifier,
) {
    val embedUrl = starIntelMapEmbedUrl(baseUrl)
    AndroidView(
        modifier = modifier,
        factory = { context ->
            WebView(context).apply {
                setBackgroundColor(android.graphics.Color.BLACK)
                isHorizontalScrollBarEnabled = false
                isVerticalScrollBarEnabled = false
                overScrollMode = WebView.OVER_SCROLL_NEVER
                webViewClient = StarIntelMapWebViewClient(baseUrl)
                settings.apply {
                    javaScriptEnabled = true
                    domStorageEnabled = true
                    allowContentAccess = false
                    allowFileAccess = false
                    javaScriptCanOpenWindowsAutomatically = false
                    mediaPlaybackRequiresUserGesture = true
                    mixedContentMode = WebSettings.MIXED_CONTENT_NEVER_ALLOW
                    safeBrowsingEnabled = true
                    setGeolocationEnabled(false)
                    setSupportMultipleWindows(false)
                }
                loadUrl(embedUrl)
            }
        },
        update = { view ->
            if (view.url.isNullOrBlank()) view.loadUrl(embedUrl)
        },
    )
}
