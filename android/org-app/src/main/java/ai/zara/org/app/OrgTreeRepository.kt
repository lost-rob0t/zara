package ai.zara.org.app

/**
 * Compatibility aliases for the first Org workbench slice.
 *
 * Storage/SAF ownership is canonical in :org-storage so Org, Org Todo,
 * Org Notebook and future focused apps cannot grow independent file models.
 */
typealias OrgFileRef = ai.zara.org.storage.OrgFileRef
typealias OrgTreeRepository = ai.zara.org.storage.OrgTreeRepository
typealias OrgTreePermission = ai.zara.org.storage.OrgTreePermission
typealias OrgHome = ai.zara.org.storage.OrgHome
typealias OrgHomeMode = ai.zara.org.storage.OrgHomeMode
typealias OrgRepository = ai.zara.org.storage.OrgRepository
