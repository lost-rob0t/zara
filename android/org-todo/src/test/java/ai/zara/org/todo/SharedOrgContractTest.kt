package ai.zara.org.todo

import ai.zara.org.core.AgendaGroup
import ai.zara.org.core.DoomAgenda
import ai.zara.org.core.OrgParser
import java.time.LocalDate
import org.junit.Assert.assertEquals
import org.junit.Test

class SharedOrgContractTest {
    @Test
    fun todoProjectionUsesCanonicalOrgCoreParserAndAgenda() {
        val source = """
            * TODO Shared parser
            SCHEDULED: <2026-09-18 Fri>
        """.trimIndent()

        val task = OrgParser.parse(source, "agenda/inbox.org").tasks.single()
        assertEquals("Shared parser", task.title)
        assertEquals(
            AgendaGroup.TODAY,
            DoomAgenda.group(task, LocalDate.of(2026, 9, 18)),
        )
    }
}
