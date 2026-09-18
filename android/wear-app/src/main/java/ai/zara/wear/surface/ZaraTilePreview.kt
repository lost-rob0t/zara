package ai.zara.wear.surface

import android.content.Context
import androidx.wear.tiles.tooling.preview.Preview
import androidx.wear.tiles.tooling.preview.TilePreviewData
import androidx.wear.tooling.preview.devices.WearDevices

@Preview(
    name = "Zara Tile — Small Round",
    group = "Zara",
    device = WearDevices.SMALL_ROUND,
)
@Preview(
    name = "Zara Tile — Large Round",
    group = "Zara",
    device = WearDevices.LARGE_ROUND,
)
fun zaraTilePreview(context: Context): TilePreviewData =
    TilePreviewData { request ->
        buildZaraTile(context, request)
    }
