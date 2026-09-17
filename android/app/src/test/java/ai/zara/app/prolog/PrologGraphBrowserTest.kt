package ai.zara.app.prolog

import java.io.File
import org.junit.Test

class PrologGraphBrowserTest {
    @Test fun allNodesAndFiles() = PrologGraphBrowserChecks.allNodesAndFilesRemainAvailable()
    @Test fun allDefinitions() = PrologGraphBrowserChecks.everyDefinitionIsPreservedAcrossFiles()
    @Test fun referencesAreNotDefinitions() = PrologGraphBrowserChecks.definitionWinsOverAnEarlierReference()
    @Test fun noDanglingFilteredEdges() = PrologGraphBrowserChecks.filtersKeepOnlyEdgesWithVisibleEndpoints()
    @Test fun emptyAndInvalidSources() = PrologGraphBrowserChecks.emptyAndInvalidSourceTextIsStillInspectable()
    @Test fun unicodeAndLineBoundaries() = PrologGraphBrowserChecks.lineOffsetsMatchOriginalUnicodeAndCrLfText()
    @Test fun noLayoutTruncation() = PrologGraphBrowserChecks.layoutIncludesEveryNodeAndRejectsGaps()
    @Test fun dirtyBufferNavigation() = PrologGraphBrowserChecks.navigationNeverDiscardsADirtyBuffer()

    @Test fun realAnalyzerResolvesCrossFileDefinitionsAndFullSource() {
        val caller = PrologSourceAnalyzer.analyze("caller.pl", "answer(X) :- target(X).\n")
        val target = PrologSourceAnalyzer.analyze("target.pl", "% heading\ntarget(ok).\n")
        val browser = PrologGraphBrowser.from(listOf(caller, target))
        val location = browser.locations("predicate:target/1").single()
        check(location == PrologGraphLocation("target.pl", 2, true))
        val source = browser.documents.single { it.source == location.source }
        val range = PrologSourceLines(source.text).ranges[location.line - 1]
        check(source.text.substring(range.start, range.endExclusive) == "target(ok).")
    }

    @Test fun graphAndSourceViewerAreWiredWithoutDiscardingDrafts() {
        val studio = File("src/main/java/ai/zara/app/ui/PrologStudioSurface.kt").readText()
        val explorer = File("src/main/java/ai/zara/app/ui/PrologGraphExplorer.kt").readText()
        check(studio.contains("PrologGraphExplorer(documents)"))
        check(studio.contains("prologCanOpenSource(selectedName, draft, selected?.text.orEmpty(), sourceName)"))
        check(studio.contains("if (sourceName != selectedName) draft = source.text"))
        check(!studio.contains("take(14)"))
        check(explorer.contains("All files ("))
        check(explorer.contains("Fit all"))
        check(explorer.contains("detectTapGestures"))
        check(explorer.contains("layout.hitTest"))
        check(explorer.contains("currentOnSelect"))
        check(explorer.contains("PrologFileViewer("))
        check(explorer.contains("PrologVisualTransformation(tokens)"))
        check(explorer.contains("scrollState.scrollToItem(activeLine - 1)"))
        check(explorer.contains("items(lines.ranges.size"))
        check(explorer.contains("clipboard.setText(AnnotatedString(document.text))"))
        check(!explorer.contains("onSaveSource"))
        check(!explorer.contains("onRunQuery"))
    }
}
