package ai.zara.app.runtime

import ai.zara.app.auth.EnrollmentState

enum class EnrollmentReadiness {
    Unenrolled,
    AwaitingServerPin,
    Ready,
    Corrupt,
}

fun EnrollmentState.toRuntimeReadiness(): EnrollmentReadiness = when (this) {
    EnrollmentState.Unenrolled -> EnrollmentReadiness.Unenrolled
    is EnrollmentState.AwaitingServerPin -> EnrollmentReadiness.AwaitingServerPin
    is EnrollmentState.Ready -> EnrollmentReadiness.Ready
    is EnrollmentState.Corrupt -> EnrollmentReadiness.Corrupt
}
