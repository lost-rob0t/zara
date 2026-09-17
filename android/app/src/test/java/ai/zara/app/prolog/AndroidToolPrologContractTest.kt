package ai.zara.app.prolog

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidToolPrologContractTest {
    @Test
    fun genericAndroidToolApiUsesAPrologEffectAndKotlinActorBoundary() {
        val plugin = File("src/main/java/ai/zara/app/prolog/AndroidToolPrologPlugin.kt")
        val actor = File("src/main/java/ai/zara/app/prolog/AndroidToolActor.kt")

        assertTrue("Android Prolog tool plugin must exist", plugin.isFile)
        assertTrue("Android tool actor must exist", actor.isFile)

        val pluginSource = plugin.readText()
        val actorSource = actor.readText()

        assertTrue(pluginSource.contains("android_tool(Action, Args, Result)"))
        assertTrue(pluginSource.contains("android_tool_action(Action, Args)"))
        assertTrue(pluginSource.contains("android_flashlight"))
        assertTrue(pluginSource.contains("android_sms_compose"))
        assertTrue(pluginSource.contains("android_location"))
        assertTrue(pluginSource.contains("android_maps"))

        assertTrue(actorSource.contains("AndroidToolEffectCodec"))
        assertTrue(actorSource.contains("DeviceCapabilityRegistry"))
        assertTrue(actorSource.contains("newSingleThreadExecutor"))
        assertFalse(actorSource.contains("System.loadLibrary"))
        assertFalse(actorSource.contains("TreallaNativeApi"))
    }

    @Test
    fun androidToolApiDoesNotAddDeviceEffectsToTreallaJni() {
        val jni = File("src/main/cpp/zara_trealla_jni.c").readText()

        assertFalse(jni.contains("android_tool"))
        assertFalse(jni.contains("DeviceCapabilityRegistry"))
        assertFalse(jni.contains("android.permission"))
    }
}
