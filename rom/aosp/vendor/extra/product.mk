# Zara/StarIntel ROM additions.
# LineageOS vendor/lineage/config/common.mk inherits vendor/extra/product.mk.

ZARA_ROM_ROOT := vendor/zara-fork/rom/aosp

PRODUCT_SOONG_NAMESPACES += \
    $(ZARA_ROM_ROOT)/prebuilts

PRODUCT_PACKAGES += \
    Zara \
    StarIntelCompanion \
    StarIntelQuasar

ifeq ($(wildcard vendor/zara-gapps/product.mk),)
$(error Missing vendor/zara-gapps/product.mk. Provide GAPPS_VENDOR_DIR before building this ROM lane)
endif

$(call inherit-product, vendor/zara-gapps/product.mk)

PRODUCT_PRODUCT_PROPERTIES += \
    ro.zara.rom=true \
    ro.zara.rom.channel=fork/android-rom
