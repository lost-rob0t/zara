package ai.zara.app.samsunghealth

import ai.zara.app.BuildConfig
import ai.zara.app.runtime.LocalQueryResult
import android.app.Activity
import android.content.Context
import java.util.concurrent.CompletableFuture

class SamsungHealthAndroidPlugin(
    private val queryProlog: (String) -> CompletableFuture<LocalQueryResult>,
    private val gateway: SamsungHealthGateway,
) : AutoCloseable {
    private val actor = SamsungHealthPluginActor(gateway)

    fun status(): CompletableFuture<SamsungHealthPluginReply> =
        run("samsung_health_status(Result)")

    fun permissions(): CompletableFuture<SamsungHealthPluginReply> =
        run("samsung_health_permissions(Result)")

    fun today(metric: SamsungHealthMetric): CompletableFuture<SamsungHealthPluginReply> =
        run("samsung_health_today(${metric.atom}, Result)")

    fun requestPermissions(
        activity: Activity,
        metrics: Set<SamsungHealthMetric> = SamsungHealthMetric.entries.toSet(),
    ): CompletableFuture<Set<SamsungHealthMetric>> {
        require(metrics.isNotEmpty()) { "At least one Samsung Health permission is required" }
        return gateway.requestPermissions(activity, metrics)
    }

    private fun run(query: String): CompletableFuture<SamsungHealthPluginReply> =
        queryProlog(query).thenCompose { result ->
            actor.dispatch(result.terms).thenApply { reply ->
                reply ?: SamsungHealthPluginReply("No Samsung Health rule matched.", false)
            }
        }

    override fun close() {
        actor.close()
    }

    companion object {
        fun create(
            context: Context,
            queryProlog: (String) -> CompletableFuture<LocalQueryResult>,
        ): SamsungHealthAndroidPlugin = SamsungHealthAndroidPlugin(
            queryProlog = queryProlog,
            gateway = SamsungHealthGatewayLoader.create(context),
        )
    }
}

object SamsungHealthGatewayLoader {
    private const val SDK_GATEWAY = "ai.zara.app.samsunghealth.sdk.SamsungHealthSdkGateway"

    fun create(context: Context): SamsungHealthGateway {
        if (!BuildConfig.HAS_SAMSUNG_HEALTH_SDK) return UnavailableSamsungHealthGateway()
        return try {
            val type = Class.forName(SDK_GATEWAY)
            val constructor = type.getConstructor(Context::class.java)
            val instance = constructor.newInstance(context.applicationContext)
            instance as? SamsungHealthGateway
                ?: UnavailableSamsungHealthGateway(SamsungHealthAvailability.ERROR)
        } catch (_: ReflectiveOperationException) {
            UnavailableSamsungHealthGateway(SamsungHealthAvailability.ERROR)
        }
    }
}