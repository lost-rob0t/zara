# Zara / StarIntel ROM product overlay.

PRODUCT_SOONG_NAMESPACES += \
    vendor/zara/android/rom/prebuilts

PRODUCT_PACKAGES += \
    ZaraSystem \
    StarIntelCompanion \
    StarIntelQuasar \
    zara_privapp_permissions

PRODUCT_PRODUCT_PROPERTIES += \
    ro.zara.rom=1 \
    ro.starintel.rom=1

# External Google-apps integration hook.
# Proprietary Google APKs are not stored in this repository.
ifdef ZARA_GAPPS_PRODUCT_MK
$(call inherit-product-if-exists,$(ZARA_GAPPS_PRODUCT_MK))
PRODUCT_PRODUCT_PROPERTIES += ro.zara.gapps_service=external
endif
