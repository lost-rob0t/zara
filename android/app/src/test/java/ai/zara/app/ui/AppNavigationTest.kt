package ai.zara.app.ui

import org.junit.Test

class AppNavigationTest {
    @Test fun threeMenus() = AppNavigationContract.threeMenus()
    @Test fun completeRouteInventory() = AppNavigationContract.completeRouteInventory()
    @Test fun everyRouteCanBeSelected() = AppNavigationContract.everyRouteCanBeSelected()
    @Test fun menuSelectionsAreIndependent() = AppNavigationContract.menuSelectionsAreIndependent()
    @Test fun savedStateRoundTrips() = AppNavigationContract.savedStateRoundTrips()
    @Test fun corruptStateFallsBackWithoutCrossMenuRoutes() = AppNavigationContract.corruptStateFallsBackWithoutCrossMenuRoutes()
    @Test fun invalidConstructionIsRejected() = AppNavigationContract.invalidConstructionIsRejected()
    @Test fun backReturnsThroughMenuRootThenChat() = AppNavigationContract.backReturnsThroughMenuRootThenChat()
    @Test fun railBreakpointUsesAvailableWindowWidth() = AppNavigationContract.railBreakpointUsesAvailableWindowWidth()
    @Test fun savedKeysAreStableNames() = AppNavigationContract.savedKeysAreStableNames()
    @Test fun stateIsSavedPerRoute() = AppNavigationWiringContract.stateIsSavedPerRoute()
    @Test fun drawerAndRailShareExactlyThreeMenus() = AppNavigationWiringContract.drawerAndRailShareExactlyThreeMenus()
    @Test fun tabsAreScrollableAndLabeled() = AppNavigationWiringContract.tabsAreScrollableAndLabeled()
    @Test fun settingsSectionsDoNotRemainOneLongForm() = AppNavigationWiringContract.settingsSectionsDoNotRemainOneLongForm()
    @Test fun adaptiveLayoutAndImeInsetsAreWired() = AppNavigationWiringContract.adaptiveLayoutAndImeInsetsAreWired()
}
