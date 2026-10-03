package ai.zara.ui.health

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class ZaraHealthCatalogTest {
    @Test
    fun phoneCatalogCoversEverySamsungHealthDataSdkReadType() {
        assertEquals(25, SamsungHealthDataMetric.entries.size)
        assertEquals(
            setOf(
                "activity_summary", "active_calories_burned_goal", "active_time_goal", "blood_glucose",
                "blood_oxygen", "blood_pressure", "body_composition", "body_temperature",
                "energy_score", "exercise", "exercise_location", "floors_climbed", "heart_rate",
                "irregular_heart_rhythm_notification", "nutrition", "nutrition_goal",
                "skin_temperature", "sleep", "sleep_apnea", "sleep_goal", "steps", "step_goal",
                "water_intake", "water_intake_goal", "user_profile",
            ),
            SamsungHealthDataMetric.entries.mapTo(linkedSetOf()) { it.atom },
        )
        assertEquals(
            SamsungHealthDataMetric.BLOOD_PRESSURE,
            SamsungHealthDataMetric.fromAtom("blood_pressure"),
        )
        assertEquals(null, SamsungHealthDataMetric.fromAtom("shell"))
    }

    @Test
    fun watchCatalogCoversEverySamsungHealthSensorSdkTracker() {
        assertEquals(12, SamsungHealthSensorTracker.entries.size)
        assertEquals(
            setOf(
                "accelerometer_continuous", "eda_continuous", "heart_rate_continuous",
                "ppg_continuous", "skin_temperature_continuous", "bia_on_demand",
                "ecg_on_demand", "mf_bia_on_demand", "ppg_on_demand",
                "skin_temperature_on_demand", "spo2_on_demand", "sweat_loss",
            ),
            SamsungHealthSensorTracker.entries.mapTo(linkedSetOf()) { it.atom },
        )
        assertEquals(
            setOf(HealthCaptureMode.CONTINUOUS, HealthCaptureMode.ON_DEMAND, HealthCaptureMode.EXERCISE),
            SamsungHealthSensorTracker.entries.mapTo(linkedSetOf()) { it.captureMode },
        )
    }

    @Test
    fun runtimeCapabilitiesNotModelNamesOwnWatchSupport() {
        val watch5Reported = setOf(
            SamsungHealthSensorTracker.ACCELEROMETER_CONTINUOUS,
            SamsungHealthSensorTracker.HEART_RATE_CONTINUOUS,
            SamsungHealthSensorTracker.PPG_CONTINUOUS,
            SamsungHealthSensorTracker.SKIN_TEMPERATURE_CONTINUOUS,
            SamsungHealthSensorTracker.BIA_ON_DEMAND,
            SamsungHealthSensorTracker.ECG_ON_DEMAND,
            SamsungHealthSensorTracker.PPG_ON_DEMAND,
            SamsungHealthSensorTracker.SKIN_TEMPERATURE_ON_DEMAND,
            SamsungHealthSensorTracker.SPO2_ON_DEMAND,
            SamsungHealthSensorTracker.SWEAT_LOSS,
        )
        val watch5 = WatchHealthCapabilities.resolve(apiLevel = 33, reported = watch5Reported)
        assertTrue(watch5.available.contains(SamsungHealthSensorTracker.SKIN_TEMPERATURE_CONTINUOUS))
        assertFalse(watch5.available.contains(SamsungHealthSensorTracker.EDA_CONTINUOUS))
        assertFalse(watch5.available.contains(SamsungHealthSensorTracker.MF_BIA_ON_DEMAND))

        val watch9Reported = SamsungHealthSensorTracker.entries.toSet()
        val watch9 = WatchHealthCapabilities.resolve(apiLevel = 36, reported = watch9Reported)
        assertEquals(watch9Reported, watch9.available)
    }

    @Test
    fun watch5SkinTemperatureRequiresAndroid13EvenWhenReported() {
        val reported = setOf(
            SamsungHealthSensorTracker.SKIN_TEMPERATURE_CONTINUOUS,
            SamsungHealthSensorTracker.SKIN_TEMPERATURE_ON_DEMAND,
        )
        val oldSoftware = WatchHealthCapabilities.resolve(apiLevel = 32, reported = reported)

        assertTrue(oldSoftware.available.isEmpty())
        assertEquals(reported, oldSoftware.updateRequired)
    }

    @Test
    fun rawSignalsAndProfilesCarryTheHighestPrivacyClass() {
        assertEquals(HealthPrivacyClass.RAW_BIOSIGNAL, SamsungHealthSensorTracker.ECG_ON_DEMAND.privacy)
        assertEquals(HealthPrivacyClass.RAW_BIOSIGNAL, SamsungHealthSensorTracker.PPG_CONTINUOUS.privacy)
        assertEquals(HealthPrivacyClass.PROFILE, SamsungHealthDataMetric.USER_PROFILE.privacy)
        assertEquals(HealthPrivacyClass.BIOMETRIC, SamsungHealthDataMetric.HEART_RATE.privacy)
    }

    @Test
    fun goalsCoverStepSleepAndEverySamsungGoalSurface() {
        assertEquals(
            setOf("steps", "sleep", "active_calories_burned", "active_time", "nutrition", "water_intake"),
            HealthGoalMetric.entries.mapTo(linkedSetOf()) { it.atom },
        )
        assertEquals(10_000, HealthGoalTarget(HealthGoalMetric.STEPS, 10_000).target)
        assertEquals(480, HealthGoalTarget(HealthGoalMetric.SLEEP, 480).target)
        assertEquals(1.25, HealthGoalTarget(HealthGoalMetric.STEPS, 10_000).progress(12_500), 0.001)
    }

    @Test(expected = IllegalArgumentException::class)
    fun healthGoalRejectsOutOfRangeTarget() {
        HealthGoalTarget(HealthGoalMetric.SLEEP, 15)
    }
}
