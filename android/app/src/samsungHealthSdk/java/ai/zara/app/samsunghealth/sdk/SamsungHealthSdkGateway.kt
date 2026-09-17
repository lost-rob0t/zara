package ai.zara.app.samsunghealth.sdk

import ai.zara.app.samsunghealth.SamsungHealthAvailability
import ai.zara.app.samsunghealth.SamsungHealthGateway
import ai.zara.app.samsunghealth.SamsungHealthMetric
import ai.zara.app.samsunghealth.SamsungHealthPlatformStatus
import ai.zara.app.samsunghealth.SamsungHealthReading
import android.app.Activity
import android.content.Context
import com.samsung.android.sdk.health.data.HealthDataService
import com.samsung.android.sdk.health.data.HealthDataStore
import com.samsung.android.sdk.health.data.error.AuthorizationException
import com.samsung.android.sdk.health.data.error.ErrorCode
import com.samsung.android.sdk.health.data.error.HealthDataException
import com.samsung.android.sdk.health.data.permission.AccessType
import com.samsung.android.sdk.health.data.permission.Permission
import com.samsung.android.sdk.health.data.request.DataType
import com.samsung.android.sdk.health.data.request.DataTypes
import com.samsung.android.sdk.health.data.request.LocalDateFilter
import com.samsung.android.sdk.health.data.request.LocalTimeFilter
import java.time.Duration
import java.time.LocalDate
import java.time.LocalDateTime
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutionException
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException
import kotlin.coroutines.Continuation
import kotlin.coroutines.EmptyCoroutineContext
import kotlin.coroutines.startCoroutine

class SamsungHealthSdkGateway(context: Context) : SamsungHealthGateway {
    private val store: HealthDataStore = HealthDataService.getStore(context.applicationContext)

    override fun status(): SamsungHealthPlatformStatus = try {
        runSuspendBlocking { store.getGrantedPermissions(allPermissions()) }
        SamsungHealthPlatformStatus(SamsungHealthAvailability.READY)
    } catch (error: HealthDataException) {
        SamsungHealthPlatformStatus(mapAvailability(error))
    } catch (_: Throwable) {
        SamsungHealthPlatformStatus(SamsungHealthAvailability.ERROR)
    }

    override fun grantedPermissions(): Set<SamsungHealthMetric> {
        val requested = permissionMap()
        val granted = runSuspendBlocking { store.getGrantedPermissions(requested.values.toSet()) }
        return requested.entries
            .filter { (_, permission) -> permission in granted }
            .mapTo(linkedSetOf()) { (metric, _) -> metric }
    }

    override fun requestPermissions(
        activity: Activity,
        metrics: Set<SamsungHealthMetric>,
    ): CompletableFuture<Set<SamsungHealthMetric>> {
        val permissions = permissionMap().filterKeys { it in metrics }
        val result = CompletableFuture<Set<SamsungHealthMetric>>()
        activity.runOnUiThread {
            startSuspend(
                block = { store.requestPermissions(permissions.values.toSet(), activity) },
                future = CompletableFuture<Set<Permission>>(),
                onComplete = { granted, error ->
                    if (error != null) {
                        result.completeExceptionally(error)
                    } else {
                        val allowed = permissions.entries
                            .filter { (_, permission) -> permission in granted.orEmpty() }
                            .mapTo(linkedSetOf()) { (metric, _) -> metric }
                        result.complete(allowed)
                    }
                },
            )
        }
        return result
    }

    override fun readToday(metric: SamsungHealthMetric): SamsungHealthReading = when (metric) {
        SamsungHealthMetric.STEPS -> readStepsToday()
        SamsungHealthMetric.SLEEP -> readSleepToday()
        SamsungHealthMetric.HEART_RATE -> readHeartRateToday()
        SamsungHealthMetric.EXERCISE -> readExerciseToday()
    }

    private fun readStepsToday(): SamsungHealthReading {
        val filter = LocalTimeFilter.of(LocalDate.now().atStartOfDay(), LocalDateTime.now())
        val request = DataType.StepsType.TOTAL.requestBuilder
            .setLocalTimeFilter(filter)
            .build()
        val response = runSuspendBlocking { store.aggregateData(request) }
        val steps = response.dataList.sumOf { it.value ?: 0L }
        return SamsungHealthReading(
            SamsungHealthMetric.STEPS,
            mapOf("steps" to steps.toString()),
        )
    }

    private fun readSleepToday(): SamsungHealthReading {
        val today = LocalDate.now()
        val request = DataType.SleepType.TOTAL_DURATION.requestBuilder
            .setLocalDateFilter(LocalDateFilter.of(today, today.plusDays(1)))
            .build()
        val response = runSuspendBlocking { store.aggregateData(request) }
        val duration = response.dataList.fold(Duration.ZERO) { total, item ->
            total.plus(item.value ?: Duration.ZERO)
        }
        return SamsungHealthReading(
            SamsungHealthMetric.SLEEP,
            mapOf("duration_minutes" to duration.toMinutes().toString()),
        )
    }

    private fun readHeartRateToday(): SamsungHealthReading {
        val today = LocalDate.now()
        val filter = LocalDateFilter.of(today, today.plusDays(1))
        val minimumRequest = DataType.HeartRateType.MIN.requestBuilder
            .setLocalDateFilter(filter)
            .build()
        val maximumRequest = DataType.HeartRateType.MAX.requestBuilder
            .setLocalDateFilter(filter)
            .build()
        val minimum = runSuspendBlocking { store.aggregateData(minimumRequest) }
            .dataList.firstNotNullOfOrNull { it.value }
        val maximum = runSuspendBlocking { store.aggregateData(maximumRequest) }
            .dataList.firstNotNullOfOrNull { it.value }
        return SamsungHealthReading(
            SamsungHealthMetric.HEART_RATE,
            buildMap {
                minimum?.let { put("min_bpm", it.toString()) }
                maximum?.let { put("max_bpm", it.toString()) }
            },
        )
    }

    private fun readExerciseToday(): SamsungHealthReading {
        val today = LocalDate.now()
        val durationRequest = DataType.ExerciseType.TOTAL_DURATION.requestBuilder
            .setLocalDateFilter(LocalDateFilter.of(today, today.plusDays(1)))
            .build()
        val caloriesRequest = DataType.ExerciseType.TOTAL_CALORIES.requestBuilder
            .setLocalTimeFilter(LocalTimeFilter.of(today.atStartOfDay(), LocalDateTime.now()))
            .build()
        val duration = runSuspendBlocking { store.aggregateData(durationRequest) }
            .dataList.fold(Duration.ZERO) { total, item -> total.plus(item.value ?: Duration.ZERO) }
        val calories = runSuspendBlocking { store.aggregateData(caloriesRequest) }
            .dataList.fold(0.0f) { total, item -> total + (item.value ?: 0.0f) }
        return SamsungHealthReading(
            SamsungHealthMetric.EXERCISE,
            mapOf(
                "duration_minutes" to duration.toMinutes().toString(),
                "calories_kcal" to calories.toString(),
            ),
        )
    }

    private fun permissionMap(): Map<SamsungHealthMetric, Permission> = linkedMapOf(
        SamsungHealthMetric.STEPS to Permission.of(DataTypes.STEPS, AccessType.READ),
        SamsungHealthMetric.SLEEP to Permission.of(DataTypes.SLEEP, AccessType.READ),
        SamsungHealthMetric.HEART_RATE to Permission.of(DataTypes.HEART_RATE, AccessType.READ),
        SamsungHealthMetric.EXERCISE to Permission.of(DataTypes.EXERCISE, AccessType.READ),
    )

    private fun allPermissions(): Set<Permission> = permissionMap().values.toSet()

    private fun mapAvailability(error: HealthDataException): SamsungHealthAvailability = when (error.errorCode) {
        ErrorCode.ERR_PLATFORM_NOT_INSTALLED -> SamsungHealthAvailability.PLATFORM_NOT_INSTALLED
        ErrorCode.ERR_OLD_VERSION_PLATFORM -> SamsungHealthAvailability.PLATFORM_TOO_OLD
        ErrorCode.ERR_PLATFORM_DISABLED -> SamsungHealthAvailability.PLATFORM_DISABLED
        ErrorCode.ERR_PLATFORM_NOT_INITIALIZED -> SamsungHealthAvailability.PLATFORM_NOT_INITIALIZED
        ErrorCode.ERR_ACCESS_CONTROL,
        ErrorCode.ERR_INVALID_PLATFORM_SIGNATURE,
        ErrorCode.ERR_CHILD_ACCOUNT_ACCESS -> SamsungHealthAvailability.AUTHORIZATION_REQUIRED
        else -> if (error is AuthorizationException) {
            SamsungHealthAvailability.AUTHORIZATION_REQUIRED
        } else {
            SamsungHealthAvailability.ERROR
        }
    }

    private fun <T> runSuspendBlocking(block: suspend () -> T): T {
        val future = CompletableFuture<T>()
        startSuspend(block, future) { _, _ -> }
        return try {
            future.get(REQUEST_TIMEOUT_SECONDS, TimeUnit.SECONDS)
        } catch (error: ExecutionException) {
            throw error.cause ?: error
        } catch (error: TimeoutException) {
            future.cancel(true)
            throw IllegalStateException("Samsung Health request timed out", error)
        }
    }

    private fun <T> startSuspend(
        block: suspend () -> T,
        future: CompletableFuture<T>,
        onComplete: (T?, Throwable?) -> Unit,
    ) {
        block.startCoroutine(object : Continuation<T> {
            override val context = EmptyCoroutineContext

            override fun resumeWith(result: Result<T>) {
                result.fold(
                    onSuccess = { value ->
                        future.complete(value)
                        onComplete(value, null)
                    },
                    onFailure = { error ->
                        future.completeExceptionally(error)
                        onComplete(null, error)
                    },
                )
            }
        })
    }

    private companion object {
        const val REQUEST_TIMEOUT_SECONDS = 20L
    }
}