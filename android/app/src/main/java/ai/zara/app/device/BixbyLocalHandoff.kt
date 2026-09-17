package ai.zara.app.device

import ai.zara.app.runtime.LocalQueryResult

class BixbyLocalHandoff(
    private val openApp: OpenAppAdapter,
) {
    fun dispatch(resolution: LocalQueryResult): DeviceActionResult? {
        val term = resolution.terms.singleOrNull() ?: return null
        if (term.filterNot(Char::isWhitespace) != BIXBY_OPEN_FRAME) return null
        return openApp.execute(DeviceActionArguments.OpenApp(BIXBY_ALIAS))
    }

    private companion object {
        const val BIXBY_ALIAS = "bixby"
        const val BIXBY_OPEN_FRAME =
            "frame(intent(ns(app),name(open)),[slot(name(target),value(ref(kind(app_alias),id(bixby))),origin(utterance))],complete)"
    }
}
