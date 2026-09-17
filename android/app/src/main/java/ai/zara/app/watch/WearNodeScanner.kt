package ai.zara.app.watch

import android.content.Context
import com.google.android.gms.wearable.CapabilityClient
import com.google.android.gms.wearable.Wearable

class WearNodeScanner(
    context: Context,
) {
    private val nodeClient = Wearable.getNodeClient(context.applicationContext)
    private val capabilityClient = Wearable.getCapabilityClient(context.applicationContext)

    fun scan(callback: (Result<List<NearbyWatch>>) -> Unit) {
        try {
            capabilityClient.getCapability(CAPABILITY, CapabilityClient.FILTER_REACHABLE)
                .addOnCompleteListener { capabilityTask ->
                    val installed = if (capabilityTask.isSuccessful) {
                        capabilityTask.result?.nodes.orEmpty().map { it.id }.toSet()
                    } else {
                        emptySet()
                    }
                    nodeClient.connectedNodes
                        .addOnSuccessListener { nodes ->
                            callback(
                                Result.success(
                                    nodes.map { node ->
                                        NearbyWatch(
                                            id = node.id,
                                            name = node.displayName,
                                            nearby = node.isNearby,
                                            zaraInstalled = node.id in installed,
                                            transport = if (node.isNearby) {
                                                "Wear Data Layer · Bluetooth nearby"
                                            } else {
                                                "Wear Data Layer"
                                            },
                                        )
                                    }.sortedByDescending { it.nearby }
                                )
                            )
                        }
                        .addOnFailureListener { error -> callback(Result.failure(error)) }
                }
        } catch (error: Exception) {
            callback(Result.failure(error))
        }
    }

    companion object {
        const val CAPABILITY = "zara_watch"
    }
}
