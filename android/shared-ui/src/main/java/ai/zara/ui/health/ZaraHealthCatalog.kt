package ai.zara.ui.health

enum class HealthSection(val label: String) {
    ACTIVITY("Activity"),
    HEART("Heart"),
    SLEEP("Sleep"),
    BODY("Body"),
    NUTRITION("Nutrition"),
    PROFILE("Profile"),
    RAW_SENSORS("Sensors"),
}

enum class HealthPrivacyClass {
    WELLNESS,
    BIOMETRIC,
    PROFILE,
    RAW_BIOSIGNAL,
}

enum class HealthCaptureMode {
    CONTINUOUS,
    ON_DEMAND,
    EXERCISE,
}

enum class HealthGoalMetric(
    val atom: String,
    val label: String,
    val unit: String,
    val minimum: Int,
    val maximum: Int,
    val step: Int,
    val defaultTarget: Int,
) {
    STEPS("steps", "Steps", "steps", 1_000, 100_000, 500, 10_000),
    SLEEP("sleep", "Sleep", "minutes", 60, 1_440, 15, 480),
    ACTIVE_CALORIES("active_calories_burned", "Active calories", "kcal", 50, 10_000, 50, 500),
    ACTIVE_TIME("active_time", "Active time", "minutes", 10, 1_440, 10, 60),
    NUTRITION("nutrition", "Nutrition", "kcal", 500, 10_000, 100, 2_000),
    WATER("water_intake", "Water", "ml", 250, 10_000, 250, 2_000);

    companion object {
        fun fromAtom(value: String): HealthGoalMetric? = entries.firstOrNull { it.atom == value }
    }
}

data class HealthGoalTarget(
    val metric: HealthGoalMetric,
    val target: Int,
) {
    init {
        require(target in metric.minimum..metric.maximum) {
            "${metric.atom} goal is outside its supported range"
        }
    }

    fun progress(current: Int): Double {
        require(current >= 0) { "health goal progress cannot be negative" }
        return current.toDouble() / target.toDouble()
    }
}

enum class SamsungHealthDataMetric(
    val atom: String,
    val label: String,
    val section: HealthSection,
    val privacy: HealthPrivacyClass = HealthPrivacyClass.WELLNESS,
) {
    ACTIVITY_SUMMARY("activity_summary", "Activity summary", HealthSection.ACTIVITY),
    ACTIVE_CALORIES_BURNED_GOAL("active_calories_burned_goal", "Active calories goal", HealthSection.ACTIVITY),
    ACTIVE_TIME_GOAL("active_time_goal", "Active time goal", HealthSection.ACTIVITY),
    BLOOD_GLUCOSE("blood_glucose", "Blood glucose", HealthSection.BODY, HealthPrivacyClass.BIOMETRIC),
    BLOOD_OXYGEN("blood_oxygen", "Blood oxygen", HealthSection.HEART, HealthPrivacyClass.BIOMETRIC),
    BLOOD_PRESSURE("blood_pressure", "Blood pressure", HealthSection.HEART, HealthPrivacyClass.BIOMETRIC),
    BODY_COMPOSITION("body_composition", "Body composition", HealthSection.BODY, HealthPrivacyClass.BIOMETRIC),
    BODY_TEMPERATURE("body_temperature", "Body temperature", HealthSection.BODY, HealthPrivacyClass.BIOMETRIC),
    ENERGY_SCORE("energy_score", "Energy score", HealthSection.BODY, HealthPrivacyClass.BIOMETRIC),
    EXERCISE("exercise", "Exercise", HealthSection.ACTIVITY),
    EXERCISE_LOCATION("exercise_location", "Exercise location", HealthSection.ACTIVITY, HealthPrivacyClass.PROFILE),
    FLOORS_CLIMBED("floors_climbed", "Floors climbed", HealthSection.ACTIVITY),
    HEART_RATE("heart_rate", "Heart rate", HealthSection.HEART, HealthPrivacyClass.BIOMETRIC),
    IRREGULAR_HEART_RHYTHM_NOTIFICATION(
        "irregular_heart_rhythm_notification",
        "Irregular rhythm notifications",
        HealthSection.HEART,
        HealthPrivacyClass.BIOMETRIC,
    ),
    NUTRITION("nutrition", "Nutrition", HealthSection.NUTRITION),
    NUTRITION_GOAL("nutrition_goal", "Nutrition goal", HealthSection.NUTRITION),
    SKIN_TEMPERATURE("skin_temperature", "Skin temperature", HealthSection.SLEEP, HealthPrivacyClass.BIOMETRIC),
    SLEEP("sleep", "Sleep", HealthSection.SLEEP, HealthPrivacyClass.BIOMETRIC),
    SLEEP_APNEA("sleep_apnea", "Sleep apnea", HealthSection.SLEEP, HealthPrivacyClass.BIOMETRIC),
    SLEEP_GOAL("sleep_goal", "Sleep goal", HealthSection.SLEEP),
    STEPS("steps", "Steps", HealthSection.ACTIVITY),
    STEP_GOAL("step_goal", "Step goal", HealthSection.ACTIVITY),
    WATER_INTAKE("water_intake", "Water intake", HealthSection.NUTRITION),
    WATER_INTAKE_GOAL("water_intake_goal", "Water goal", HealthSection.NUTRITION),
    USER_PROFILE("user_profile", "User profile", HealthSection.PROFILE, HealthPrivacyClass.PROFILE);

    companion object {
        fun fromAtom(value: String): SamsungHealthDataMetric? = entries.firstOrNull { it.atom == value }
    }
}

enum class SamsungHealthSensorTracker(
    val atom: String,
    val label: String,
    val captureMode: HealthCaptureMode,
    val privacy: HealthPrivacyClass,
    val minimumApiLevel: Int = 30,
) {
    ACCELEROMETER_CONTINUOUS(
        "accelerometer_continuous", "Motion", HealthCaptureMode.CONTINUOUS, HealthPrivacyClass.RAW_BIOSIGNAL,
    ),
    EDA_CONTINUOUS(
        "eda_continuous", "Electrodermal activity", HealthCaptureMode.CONTINUOUS, HealthPrivacyClass.RAW_BIOSIGNAL,
    ),
    HEART_RATE_CONTINUOUS(
        "heart_rate_continuous", "Heart rate + IBI", HealthCaptureMode.CONTINUOUS, HealthPrivacyClass.BIOMETRIC,
    ),
    PPG_CONTINUOUS(
        "ppg_continuous", "PPG", HealthCaptureMode.CONTINUOUS, HealthPrivacyClass.RAW_BIOSIGNAL,
    ),
    SKIN_TEMPERATURE_CONTINUOUS(
        "skin_temperature_continuous", "Skin temperature", HealthCaptureMode.CONTINUOUS,
        HealthPrivacyClass.BIOMETRIC, minimumApiLevel = 33,
    ),
    BIA_ON_DEMAND(
        "bia_on_demand", "Body composition", HealthCaptureMode.ON_DEMAND, HealthPrivacyClass.BIOMETRIC,
    ),
    ECG_ON_DEMAND(
        "ecg_on_demand", "ECG", HealthCaptureMode.ON_DEMAND, HealthPrivacyClass.RAW_BIOSIGNAL,
    ),
    MF_BIA_ON_DEMAND(
        "mf_bia_on_demand", "Multi-frequency BIA", HealthCaptureMode.ON_DEMAND, HealthPrivacyClass.RAW_BIOSIGNAL,
    ),
    PPG_ON_DEMAND(
        "ppg_on_demand", "PPG spot check", HealthCaptureMode.ON_DEMAND, HealthPrivacyClass.RAW_BIOSIGNAL,
    ),
    SKIN_TEMPERATURE_ON_DEMAND(
        "skin_temperature_on_demand", "Skin temperature spot check", HealthCaptureMode.ON_DEMAND,
        HealthPrivacyClass.BIOMETRIC, minimumApiLevel = 33,
    ),
    SPO2_ON_DEMAND(
        "spo2_on_demand", "Blood oxygen", HealthCaptureMode.ON_DEMAND, HealthPrivacyClass.BIOMETRIC,
    ),
    SWEAT_LOSS(
        "sweat_loss", "Running sweat loss", HealthCaptureMode.EXERCISE, HealthPrivacyClass.BIOMETRIC,
    );

    companion object {
        fun fromAtom(value: String): SamsungHealthSensorTracker? = entries.firstOrNull { it.atom == value }
    }
}

data class WatchHealthCapabilities(
    val available: Set<SamsungHealthSensorTracker>,
    val updateRequired: Set<SamsungHealthSensorTracker>,
    val unsupported: Set<SamsungHealthSensorTracker>,
) {
    companion object {
        fun resolve(
            apiLevel: Int,
            reported: Set<SamsungHealthSensorTracker>,
        ): WatchHealthCapabilities {
            require(apiLevel > 0) { "apiLevel must be positive" }
            val updateRequired = reported.filterTo(linkedSetOf()) { apiLevel < it.minimumApiLevel }
            val available = reported.filterTo(linkedSetOf()) { apiLevel >= it.minimumApiLevel }
            return WatchHealthCapabilities(
                available = available,
                updateRequired = updateRequired,
                unsupported = SamsungHealthSensorTracker.entries.filterTo(linkedSetOf()) { it !in reported },
            )
        }
    }
}
