                {"enabled": True, "disabled_providers": []},
                engine=PrologEngine(tmp_path / "missing.pl"),
            )


class TestPlanExecutionService:
    @pytest.fixture(autouse=True)
    def _service_engine(self, engine):
        self._engine = engine

    def service(self, **kwargs) -> PlanExecutionService:
        return build_api_service(
            {"enabled": True, "disabled_providers": []}, engine=self._engine, **kwargs
        )

    def test_search_plan_executes_and_returns_url(self):
        service = self.service()
        frame = complete_frame("web", "search", text_slot("query", "prolog test"))
        outcome = asyncio.run(
            service.execute(
                frame, principal="alice", auths=(), request_id="req-search-1"
            )
        )
        assert outcome.status is PlanOutcomeStatus.EXECUTED
        assert "search.brave.com" in outcome.response

    def test_open_app_plan_is_unavailable_on_headless_server(self):
        service = self.service()
        frame = complete_frame(
            "app", "open", FilledSlot(
                name="target",
                value=RefValue(kind="app_alias", id="firefox"),
                origin=SlotOrigin.UTTERANCE,
            )
        )
        outcome = asyncio.run(
            service.execute(frame, principal="alice", auths=(), request_id="req-open-1")
        )
        assert outcome.status is PlanOutcomeStatus.REFUSED
        assert outcome.detail == "plan_not_ready"
        assert "unavailable" in outcome.response