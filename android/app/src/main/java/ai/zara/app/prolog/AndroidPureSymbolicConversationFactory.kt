package ai.zara.app.prolog

import ai.zara.app.AndroidAppSession
import ai.zara.app.expert.CanonicalExpertInvocationPort
import ai.zara.app.expert.ExpertLimits
import ai.zara.app.expert.PureSymbolicExpertConversationResult
import ai.zara.app.expert.PureSymbolicExpertInvocationAdapter
import ai.zara.app.history.ConversationHistoryContract
import ai.zara.app.history.HistoryMessageRole
import ai.zara.app.history.HistoryMessageStatus
import ai.zara.app.history.PortableConversationStore
import ai.zara.app.history.SymbolicConversationProjection
import ai.zara.app.history.SymbolicProjectScopeContract
import ai.zara.app.history.completeSymbolicTurnAtomically
import ai.zara.app.history.failSymbolicTurnBeforeProjection
import ai.zara.app.history.loadSymbolicProjection
import ai.zara.app.history.saveSymbolicProjection
import ai.zara.app.history.toSymbolicEdgeSnapshot
import ai.zara.app.runtime.LocalQueryResult
import java.util.concurrent.CompletableFuture

internal object AndroidPureSymbolicConversationFactory {
    fun create(
        session: AndroidAppSession,
        projectionStore: PortableConversationStore,
        projectIdForConversation: (String) -> String? = { null },
        canonicalExpertInvocationPort: CanonicalExpertInvocationPort? = null,
    ): PureSymbolicConversationController =
        controller(
            session = session,
            resolve = { utterance, conversationId ->
                resolvePersistedTurn(
                    session = session,
                    projectionStore = projectionStore,
                    utterance = utterance,
                    conversationId = conversationId,
                    requestedProjectId = { projectIdForConversation(conversationId) },
                    canonicalExpertInvocationPort = canonicalExpertInvocationPort,
                )
            },
        )

    private fun controller(
        session: AndroidAppSession,
        resolve: (String, String) -> PureSymbolicResolution,
    ): PureSymbolicConversationController =
        PureSymbolicConversationController(
            catalog = { PrologWorkspaceCatalog.from(session.prologSources()) },
            query = session::queryLocalProlog,
            resolve = resolve,
        )

    private fun resolvePersistedTurn(
        session: AndroidAppSession,
        projectionStore: PortableConversationStore,
        utterance: String,
        conversationId: String,
        requestedProjectId: () -> String?,
        canonicalExpertInvocationPort: CanonicalExpertInvocationPort?,
    ): PureSymbolicResolution {
        val turnId = requireRunningTurnId(projectionStore, conversationId)
        val prepared = try {
            val current = projectionStore.loadSymbolicProjection(conversationId)
            current?.assertPureSymbolic()
            val projectScope = SymbolicProjectScopeContract.next(current, requestedProjectId())
            val expectedGeneration = current?.projectionGeneration ?: 0L
            val resetProjectKnowledge = current != null && projectScope.projectId != current.projectId
            val context0 = if (resetProjectKnowledge) {
                SymbolicDialogueContextCodec.emptyContextTerm
            } else {
                SymbolicDialogueContextCodec.decode(current?.dialogueStateJson ?: "{}")
            }
            val previousExpertResponseAct = if (resetProjectKnowledge) {
                null
            } else {
                previousExpertResponseAct(
                    current = current,
                    projectionStore = projectionStore,
                    conversationId = conversationId,
                )
            }
            PreparedTurn(
                current = current,
                expectedGeneration = expectedGeneration,
                context0 = context0,
                projectId = projectScope.projectId,
                projectGeneration = projectScope.projectGeneration,
                resetProjectKnowledge = resetProjectKnowledge,
                previousExpertResponseAct = previousExpertResponseAct,
            )
        } catch (error: Throwable) {
            return PureSymbolicResolution(
                turnId = turnId,
                future = failBeforePendingProjection(
                    projectionStore = projectionStore,
                    conversationId = conversationId,
                    turnId = turnId,
                    error = error,
                ),
            )
        }

        val pendingProjection = try {
            pendingProjection(
                current = prepared.current,
                conversationId = conversationId,
                expectedGeneration = prepared.expectedGeneration,
                context0 = prepared.context0,
                turnId = turnId,
                projectId = prepared.projectId,
                projectGeneration = prepared.projectGeneration,
                resetProjectKnowledge = prepared.resetProjectKnowledge,
            ).also { pending ->
                projectionStore.saveSymbolicProjection(
                    projection = pending,
                    expectedGeneration = prepared.expectedGeneration,
                )
            }
        } catch (error: Throwable) {
            return PureSymbolicResolution(
                turnId = turnId,
                future = failBeforePendingProjection(
                    projectionStore = projectionStore,
                    conversationId = conversationId,
                    turnId = turnId,
                    error = error,
                ),
            )
        }
        val pendingGeneration = pendingProjection.projectionGeneration

        val turnFuture: CompletableFuture<LocalQueryResult> = try {
            val catalog = PrologWorkspaceCatalog.from(session.prologSources())
            val selection = LocalNaturalLanguageExpertRouter.select(utterance, catalog)
            if (selection != null) {
                val port = canonicalExpertInvocationPort
                    ?: throw IllegalStateException("Canonical expert owner is unavailable")
                CanonicalNaturalExpertTurn(
                    PureSymbolicExpertInvocationAdapter(port),
                ).invoke(
                    selection = selection,
                    principal = ConversationHistoryContract.localPrincipalId,
                    workspace = prepared.projectId ?: DEFAULT_EXPERT_WORKSPACE,
                    requestId = turnId,
                    limits = PURE_SYMBOLIC_EXPERT_LIMITS,
                    idempotencyKey = "$turnId:${selection.expertOperation}",
                ).thenApply { projected ->
                    canonicalExpertEnvelopeResult(
                        projected = projected,
                        contextTerm = prepared.context0,
                        generation = pendingProjection.runtimeGeneration,
                    )
                }
            } else {
                val query = if (prepared.previousExpertResponseAct != null) {
                    discourseAwareDialogueTurnEnvelopeQuery(
                        utterance = utterance,
                        contextTerm = prepared.context0,
                        previousSummary = prepared.previousExpertResponseAct.summary,
                        previousEvidenceRef = prepared.previousExpertResponseAct.evidenceRef,
                    )
                } else {
                    dialogueTurnEnvelopeQuery(utterance, prepared.context0)
                }
                session.queryLocalProlog(query)
            }
        } catch (error: Throwable) {
            return PureSymbolicResolution(
                turnId = turnId,
                future = failBeforeAsyncEvaluation(
                    projectionStore = projectionStore,
                    pendingProjection = pendingProjection,
                    pendingGeneration = pendingGeneration,
                    context0 = prepared.context0,
                    error = error,
                ),
            )
        }
        val output = PersistenceFencedFuture(turnFuture) {
            cancelPersistedTurnIfOwned(
                projectionStore = projectionStore,
                pendingProjection = pendingProjection,
                pendingGeneration = pendingGeneration,
                context0 = prepared.context0,
            )
        }

        turnFuture.whenComplete { result, resultError ->
            if (output.isDone) return@whenComplete
            if (resultError != null || result == null) {
                val failure = resultError
                    ?: IllegalStateException("Symbolic dialogue result is missing")
                completeFailure(
                    output = output,
                    projectionStore = projectionStore,
                    pendingProjection = pendingProjection,
                    pendingGeneration = pendingGeneration,
                    context0 = prepared.context0,
                    error = failure,
                )
                return@whenComplete
            }
            if (result.terms.isEmpty()) {
                completeNoMatch(
                    output = output,
                    projectionStore = projectionStore,
                    pendingProjection = pendingProjection,
                    pendingGeneration = pendingGeneration,
                    context0 = prepared.context0,
                    result = result,
                )
                return@whenComplete
            }

            try {
                val envelope = splitDialogueEnvelope(result)
                val completedProjection = terminalProjection(
                    pending = pendingProjection,
                    contextTerm = envelope.contextTerm,
                    outcome = "success",
                    dialogueAct = envelope.dialogueAct,
                    expertEvidenceRef = envelope.expertEvidenceRef,
                )
                output.commitOrCancel {
                    projectionStore.completeSymbolicTurnAtomically(
                        projection = completedProjection,
                        expectedGeneration = pendingGeneration,
                        turnId = turnId,
                        assistantContent = envelope.renderedResponse,
                        assistantStatus = HistoryMessageStatus.Complete,
                    )
                    output.complete(result.copy(terms = listOf(envelope.renderedResponse)))
                }
            } catch (error: Throwable) {
                completeFailure(
                    output = output,
                    projectionStore = projectionStore,
                    pendingProjection = pendingProjection,
                    pendingGeneration = pendingGeneration,
                    context0 = prepared.context0,
                    error = error,
                )
            }
        }
        return PureSymbolicResolution(turnId = turnId, future = output)
    }

    private data class PreparedTurn(
        val current: SymbolicConversationProjection?,
        val expectedGeneration: Long,
        val context0: String,
        val projectId: String?,
        val projectGeneration: Long,
        val resetProjectKnowledge: Boolean,
        val previousExpertResponseAct: PreviousExpertResponseAct?,
    )

    private data class PreviousExpertResponseAct(
        val summary: String,
        val evidenceRef: String,
    )

    private data class DialogueEnvelope(
        val renderedResponse: String,
        val contextTerm: String,
        val dialogueAct: String,
        val expertEvidenceRef: String?,
    )

    private fun previousExpertResponseAct(
        current: SymbolicConversationProjection?,
        projectionStore: PortableConversationStore,
        conversationId: String,
    ): PreviousExpertResponseAct? {
        if (current == null || current.outcome != "success" || current.dialogueAct != "expert_answer") {
            return null
        }
        val evidenceRefs = current.toSymbolicEdgeSnapshot(
            ConversationHistoryContract.localPrincipalId,
        ).expertEvidenceRefs
        require(evidenceRefs.size == 1) {
            "Canonical expert answer must expose exactly one stable evidence reference"
        }
        val previousTurnId = requireNotNull(current.turnId) {
            "Canonical expert answer is missing its turn identity"
        }
        val summary = projectionStore.loadMessages(conversationId).lastOrNull { message ->
            message.role == HistoryMessageRole.Assistant &&
                message.status == HistoryMessageStatus.Complete &&
                message.turnId == previousTurnId
        }?.content ?: error("Canonical expert answer is missing its durable assistant message")
        require(summary.isNotBlank() && summary.length <= MAX_EXPERT_SUMMARY_CHARS) {
            "Canonical expert summary is outside the symbolic discourse bound"
        }
        require(summary.none { character ->
            character.isISOControl() && character !in charArrayOf('\n', '\r', '\t')
        }) {
            "Canonical expert summary contains unsupported control characters"
        }
        return PreviousExpertResponseAct(
            summary = summary,
            evidenceRef = requireExpertEvidenceRef(evidenceRefs.single()),
        )
    }

    private fun requireRunningTurnId(
        projectionStore: PortableConversationStore,
        conversationId: String,
    ): String {
        val assistant = projectionStore.loadMessages(conversationId).lastOrNull { message ->
            message.role == HistoryMessageRole.Assistant &&
                (message.status == HistoryMessageStatus.Pending ||
                    message.status == HistoryMessageStatus.Streaming)
        } ?: error("Pure-symbolic conversation has no running canonical turn")
        return requireNotNull(assistant.turnId) {
            "Pure-symbolic running turn is missing canonical turn identity"
        }
    }

    private fun failBeforePendingProjection(
        projectionStore: PortableConversationStore,
        conversationId: String,
        turnId: String,
        error: Throwable,
    ): CompletableFuture<LocalQueryResult> = try {
        projectionStore.failSymbolicTurnBeforeProjection(
            conversationId = conversationId,
            turnId = turnId,
            assistantContent = RUNTIME_FAILURE_TEXT,
            assistantError = RUNTIME_FAILURE_TEXT,
        )
        CompletableFuture.failedFuture(error)
    } catch (fenceError: Throwable) {
        CompletableFuture.failedFuture(fenceError)
    }

    private fun failBeforeAsyncEvaluation(
        projectionStore: PortableConversationStore,
        pendingProjection: SymbolicConversationProjection,
        pendingGeneration: Long,
        context0: String,
        error: Throwable,
    ): CompletableFuture<LocalQueryResult> {
        val terminal = terminalProjection(
            pending = pendingProjection,
            contextTerm = context0,
            outcome = "error",
        )
        return try {
            projectionStore.completeSymbolicTurnAtomically(
                projection = terminal,
                expectedGeneration = pendingGeneration,
                turnId = requireNotNull(pendingProjection.turnId),
                assistantContent = RUNTIME_FAILURE_TEXT,
                assistantStatus = HistoryMessageStatus.Error,
                assistantError = RUNTIME_FAILURE_TEXT,
            )
            CompletableFuture.failedFuture(error)
        } catch (fenceError: Throwable) {
            CompletableFuture.failedFuture(fenceError)
        }
    }

    private fun cancelPersistedTurnIfOwned(
        projectionStore: PortableConversationStore,
        pendingProjection: SymbolicConversationProjection,
        pendingGeneration: Long,
        context0: String,
    ) {
        try {
            projectionStore.completeSymbolicTurnAtomically(
                projection = terminalProjection(
                    pending = pendingProjection,
                    contextTerm = context0,
                    outcome = "cancelled",
                ),
                expectedGeneration = pendingGeneration,
                turnId = requireNotNull(pendingProjection.turnId),
                assistantContent = "",
                assistantStatus = HistoryMessageStatus.Cancelled,
            )
        } catch (error: Throwable) {
            val current = try {
                projectionStore.loadSymbolicProjection(pendingProjection.conversationId)
            } catch (_: Throwable) {
                throw error
            }
            val stillOwnsPendingGeneration = current != null &&
                current.turnId == pendingProjection.turnId &&
                current.outcome == "pending" &&
                current.projectionGeneration == pendingGeneration
            if (stillOwnsPendingGeneration) throw error
        }
    }

    private fun completeFailure(
        output: PersistenceFencedFuture,
        projectionStore: PortableConversationStore,
        pendingProjection: SymbolicConversationProjection,
        pendingGeneration: Long,
        context0: String,
        error: Throwable,
    ) {
        try {
            output.commitOrCancel {
                projectionStore.completeSymbolicTurnAtomically(
                    projection = terminalProjection(
                        pending = pendingProjection,
                        contextTerm = context0,
                        outcome = "error",
                    ),
                    expectedGeneration = pendingGeneration,
                    turnId = requireNotNull(pendingProjection.turnId),
                    assistantContent = RUNTIME_FAILURE_TEXT,
                    assistantStatus = HistoryMessageStatus.Error,
                    assistantError = RUNTIME_FAILURE_TEXT,
                )
                output.completeExceptionally(error)
            }
        } catch (fenceError: Throwable) {
            output.completeExceptionally(fenceError)
        }
    }

    private fun completeNoMatch(
        output: PersistenceFencedFuture,
        projectionStore: PortableConversationStore,
        pendingProjection: SymbolicConversationProjection,
        pendingGeneration: Long,
        context0: String,
        result: LocalQueryResult,
    ) {
        try {
            output.commitOrCancel {
                projectionStore.completeSymbolicTurnAtomically(
                    projection = terminalProjection(
                        pending = pendingProjection,
                        contextTerm = context0,
                        outcome = "error",
                    ),
                    expectedGeneration = pendingGeneration,
                    turnId = requireNotNull(pendingProjection.turnId),
                    assistantContent = NO_MATCH_TEXT,
                    assistantStatus = HistoryMessageStatus.Error,
                    assistantError = NO_MATCH_TEXT,
                )
                output.complete(result)
            }
        } catch (fenceError: Throwable) {
            output.completeExceptionally(fenceError)
        }
    }

    private fun pendingProjection(
        current: SymbolicConversationProjection?,
        conversationId: String,
        expectedGeneration: Long,
        context0: String,
        turnId: String,
        projectId: String?,
        projectGeneration: Long,
        resetProjectKnowledge: Boolean,
    ): SymbolicConversationProjection {
        val runtimeGeneration = current?.runtimeGeneration?.let { Math.addExact(it, 1L) } ?: 1L
        val base = current ?: SymbolicConversationProjection(
            conversationId = conversationId,
            projectionGeneration = expectedGeneration + 1L,
            runtimeGeneration = runtimeGeneration,
            dialogueStateJson = SymbolicDialogueContextCodec.encode(context0),
        )
        return base.copy(
            projectionGeneration = expectedGeneration + 1L,
            runtimeGeneration = runtimeGeneration,
            turnId = turnId,
            outcome = "pending",
            projectId = projectId,
            projectGeneration = projectGeneration,
            dialogueAct = "conversation",
            dialogueStateJson = SymbolicDialogueContextCodec.encode(context0),
            discourseEntitiesJson = if (resetProjectKnowledge) "[]" else base.discourseEntitiesJson,
            unresolvedQuestionsJson = if (resetProjectKnowledge) "[]" else base.unresolvedQuestionsJson,
            expertEvidenceJson = if (resetProjectKnowledge) "[]" else base.expertEvidenceJson,
            verifiedFactsJson = if (resetProjectKnowledge) "[]" else base.verifiedFactsJson,
            rendererProvenance = "",
            providersEnabled = false,
            maxModelCalls = 0L,
            providerCalls = 0L,
            modelCalls = 0L,
        ).also(SymbolicConversationProjection::assertPureSymbolic)
    }

    private fun terminalProjection(
        pending: SymbolicConversationProjection,
        contextTerm: String,
        outcome: String,
        dialogueAct: String? = null,
        expertEvidenceRef: String? = null,
    ): SymbolicConversationProjection = pending.copy(
        projectionGeneration = Math.addExact(pending.projectionGeneration, 1L),
        outcome = outcome,
        dialogueAct = when (outcome) {
            "success" -> requireNotNull(dialogueAct) {
                "Successful symbolic terminal projection is missing its canonical dialogue act"
            }
            "error" -> "error"
            "cancelled" -> "cancelled"
            else -> error("Unsupported symbolic terminal outcome: $outcome")
        },
        dialogueStateJson = SymbolicDialogueContextCodec.encode(contextTerm),
        expertEvidenceJson = if (outcome == "success" && dialogueAct == "expert_answer") {
            expertEvidenceJson(requireNotNull(expertEvidenceRef) {
                "Canonical expert answer is missing expert evidence"
            })
        } else {
            pending.expertEvidenceJson
        },
        rendererProvenance = "symbolic-dcg/v1",
        providersEnabled = false,
        maxModelCalls = 0L,
        providerCalls = 0L,
        modelCalls = 0L,
    ).also(SymbolicConversationProjection::assertPureSymbolic)

    private fun splitDialogueEnvelope(result: LocalQueryResult): DialogueEnvelope {
        check(result.terms.size == 4) {
            "Symbolic dialogue must return one rendered response, one canonical Context1 term, one dialogue act, and one expert-evidence wire"
        }
        val renderedResponse = result.terms[0]
        val contextWire = result.terms[1]
        val actWire = result.terms[2]
        val expertEvidenceWire = result.terms[3]
        check(contextWire.startsWith(DIALOGUE_CONTEXT_WIRE_PREFIX)) {
            "Symbolic dialogue Context1 result has an invalid wire prefix"
        }
        check(actWire.startsWith(DIALOGUE_ACT_WIRE_PREFIX)) {
            "Symbolic dialogue act result has an invalid wire prefix"
        }
        check(expertEvidenceWire.startsWith(DIALOGUE_EXPERT_EVIDENCE_WIRE_PREFIX)) {
            "Symbolic dialogue expert-evidence result has an invalid wire prefix"
        }
        val contextTerm = contextWire.removePrefix(DIALOGUE_CONTEXT_WIRE_PREFIX)
        val dialogueAct = requireDialogueActName(actWire.removePrefix(DIALOGUE_ACT_WIRE_PREFIX))
        val rawExpertEvidence = expertEvidenceWire.removePrefix(DIALOGUE_EXPERT_EVIDENCE_WIRE_PREFIX)
        val expertEvidenceRef = if (dialogueAct == "expert_answer") {
            requireExpertEvidenceRef(rawExpertEvidence)
        } else {
            require(rawExpertEvidence.isEmpty()) {
                "Non-expert symbolic dialogue returned expert evidence"
            }
            null
        }
        return DialogueEnvelope(
            renderedResponse = renderedResponse,
            contextTerm = SymbolicDialogueContextCodec.requireContextTerm(contextTerm),
            dialogueAct = dialogueAct,
            expertEvidenceRef = expertEvidenceRef,
        )
    }

    private fun requireDialogueActName(raw: String): String {
        require(raw.isNotEmpty()) { "Symbolic dialogue act is required" }
        require(raw.length <= MAX_DIALOGUE_ACT_CHARS) { "Symbolic dialogue act is too large" }
        require(raw.all { character -> character.isLetterOrDigit() || character == '_' }) {
            "Symbolic dialogue act contains invalid characters"
        }
        return raw
    }

    private fun requireExpertEvidenceRef(raw: String): String {
        require(raw.isNotBlank()) { "Canonical expert answer requires expert evidence" }
        require(raw.length <= MAX_EXPERT_EVIDENCE_CHARS) { "Expert evidence reference is too large" }
        require(raw.none(Char::isISOControl)) { "Expert evidence reference contains control characters" }
        return raw
    }

    private fun expertEvidenceJson(reference: String): String = buildString(reference.length + 16) {
        append("[{\"ref\":\"")
        reference.forEach { character ->
            when (character) {
                '\\' -> append("\\\\")
                '"' -> append("\\\"")
                '\b' -> append("\\b")
                '\u000c' -> append("\\f")
                '\n' -> append("\\n")
                '\r' -> append("\\r")
                '\t' -> append("\\t")
                else -> {
                    if (character.code < 0x20) {
                        append("\\u")
                        append(character.code.toString(16).padStart(4, '0'))
                    } else {
                        append(character)
                    }
                }
            }
        }
        append("\"}]")
    }

    private class PersistenceFencedFuture(
        private val upstream: CompletableFuture<*>,
        private val onCancel: () -> Unit,
    ) : CompletableFuture<LocalQueryResult>() {
        private val fence = Any()

        fun commitOrCancel(commit: () -> Unit) = synchronized(fence) {
            if (!isDone) commit()
        }

        override fun cancel(mayInterruptIfRunning: Boolean): Boolean = synchronized(fence) {
            if (isDone) return@synchronized false
            onCancel()
            val cancelled = super.cancel(mayInterruptIfRunning)
            if (cancelled) upstream.cancel(mayInterruptIfRunning)
            cancelled
        }
    }

    internal fun dialogueTurnQuery(
        utterance: String,
        contextTerm: String = SymbolicDialogueContextCodec.emptyContextTerm,
    ): String = dialogueTurnPrelude(utterance, contextTerm) +
        ", symbolic_dialogue:render_response(Act, Result)"

    internal fun dialogueTurnEnvelopeQuery(
        utterance: String,
        contextTerm: String,
    ): String = "((" + dialogueTurnPrelude(utterance, contextTerm) +
        ", symbolic_dialogue:render_response(Act, Response), " +
        "write_term_to_atom(ContextAtom, Context1, [quoted(true)]), " +
        "atom_concat('$DIALOGUE_CONTEXT_WIRE_PREFIX', ContextAtom, ContextTagged), " +
        "atom_codes(ContextTagged, ContextWireCodes), " +
        "string_codes(ContextWire, ContextWireCodes), " +
        "(Act = answer(expert, _, evidence(EvidenceRef)) -> " +
        "ActName = expert_answer, " +
        "(atom(EvidenceRef) -> atom_codes(EvidenceRef, EvidenceCodes) ; " +
        "string(EvidenceRef) -> string_codes(EvidenceRef, EvidenceCodes) ; fail) ; " +
        "functor(Act, ActName, _), EvidenceCodes = []), " +
        "atom_concat('$DIALOGUE_ACT_WIRE_PREFIX', ActName, ActTagged), " +
        "atom_codes(ActTagged, ActWireCodes), " +
        "string_codes(ActWire, ActWireCodes), " +
        "string_codes(\"$DIALOGUE_EXPERT_EVIDENCE_WIRE_PREFIX\", EvidencePrefixCodes), " +
        "append(EvidencePrefixCodes, EvidenceCodes, EvidenceWireCodes), " +
        "string_codes(EvidenceWire, EvidenceWireCodes)) -> true ; fail), " +
        "(Result = Response ; Result = ContextWire ; Result = ActWire ; Result = EvidenceWire)"

    internal fun canonicalExpertEnvelopeResult(
        projected: PureSymbolicExpertConversationResult,
        contextTerm: String,
        generation: Long,
    ): LocalQueryResult {
        require(generation >= 0L) { "Canonical expert result generation must be non-negative" }
        val canonicalContext = SymbolicDialogueContextCodec.requireContextTerm(contextTerm)
        val evidenceRef = requireExpertEvidenceRef(projected.evidenceRef)
        return LocalQueryResult(
            query = CANONICAL_EXPERT_QUERY_MARKER,
            terms = listOf(
                projected.summary,
                "$DIALOGUE_CONTEXT_WIRE_PREFIX$canonicalContext",
                "${DIALOGUE_ACT_WIRE_PREFIX}expert_answer",
                "$DIALOGUE_EXPERT_EVIDENCE_WIRE_PREFIX$evidenceRef",
            ),
            generation = generation,
        )
    }

    internal fun discourseAwareDialogueTurnEnvelopeQuery(
        utterance: String,
        contextTerm: String,
        previousSummary: String,
        previousEvidenceRef: String,
    ): String {
        val text = utterance.trim()
        require(text.isNotEmpty()) { "Utterance is required" }
        require(text.length <= MAX_UTTERANCE_CHARS) { "Utterance is too large" }
        require(previousSummary.isNotBlank() && previousSummary.length <= MAX_EXPERT_SUMMARY_CHARS) {
            "Canonical expert summary is outside the symbolic discourse bound"
        }
        val escapedText = prologString(text)
        val escapedSummary = prologString(previousSummary)
        val escapedEvidence = prologString(requireExpertEvidenceRef(previousEvidenceRef))
        val canonicalContext = SymbolicDialogueContextCodec.requireContextTerm(contextTerm)
        val escapedContext = SymbolicDialogueContextCodec.prologString(canonicalContext)
        return "((string_codes(\"$escapedContext\", Context0Codes), " +
            "atom_codes(Context0Atom, Context0Codes), " +
            "read_term_from_atom(Context0Atom, Context0, []), " +
            "symbolic_dialogue_turn:valid_dialogue_context(Context0), " +
            "string_codes(\"$escapedSummary\", PreviousSummaryCodes), " +
            "string_codes(PreviousSummary, PreviousSummaryCodes), " +
            "string_codes(\"$escapedEvidence\", PreviousEvidenceCodes), " +
            "string_codes(PreviousEvidenceRef, PreviousEvidenceCodes), " +
            "PreviousAct = answer(expert, PreviousSummary, evidence(PreviousEvidenceRef)), " +
            "((symbolic_dialogue:resolve_discourse(\"$escapedText\", PreviousAct, DiscourseAct), " +
            "DiscourseAct \\= unsupported) -> " +
            "Act = DiscourseAct, Context1 = Context0 ; " +
            "symbolic_dialogue_turn:dialogue_turn(\"$escapedText\", conversation, Context0, " +
            "turn(_Frames, Act, Context1))), " +
            "symbolic_dialogue_turn:valid_dialogue_context(Context1), " +
            "symbolic_dialogue:render_response(Act, Response), " +
            "write_term_to_atom(ContextAtom, Context1, [quoted(true)]), " +
            "atom_concat('$DIALOGUE_CONTEXT_WIRE_PREFIX', ContextAtom, ContextTagged), " +
            "atom_codes(ContextTagged, ContextWireCodes), " +
            "string_codes(ContextWire, ContextWireCodes), " +
            "(Act = answer(expert, _, evidence(EvidenceRef)) -> " +
            "ActName = expert_answer, " +
            "(atom(EvidenceRef) -> atom_codes(EvidenceRef, EvidenceCodes) ; " +
            "string(EvidenceRef) -> string_codes(EvidenceRef, EvidenceCodes) ; fail) ; " +
            "functor(Act, ActName, _), EvidenceCodes = []), " +
            "atom_concat('$DIALOGUE_ACT_WIRE_PREFIX', ActName, ActTagged), " +
            "atom_codes(ActTagged, ActWireCodes), " +
            "string_codes(ActWire, ActWireCodes), " +
            "string_codes(\"$DIALOGUE_EXPERT_EVIDENCE_WIRE_PREFIX\", EvidencePrefixCodes), " +
            "append(EvidencePrefixCodes, EvidenceCodes, EvidenceWireCodes), " +
            "string_codes(EvidenceWire, EvidenceWireCodes)) -> true ; fail), " +
            "(Result = Response ; Result = ContextWire ; Result = ActWire ; Result = EvidenceWire)"
    }

    private fun dialogueTurnPrelude(utterance: String, contextTerm: String): String {
        val text = utterance.trim()
        require(text.isNotEmpty()) { "Utterance is required" }
        require(text.length <= MAX_UTTERANCE_CHARS) { "Utterance is too large" }
        val escapedText = prologString(text)
        val canonicalContext = SymbolicDialogueContextCodec.requireContextTerm(contextTerm)
        val escapedContext = SymbolicDialogueContextCodec.prologString(canonicalContext)
        return "string_codes(\"$escapedContext\", Context0Codes), " +
            "atom_codes(Context0Atom, Context0Codes), " +
            "read_term_from_atom(Context0Atom, Context0, []), " +
            "symbolic_dialogue_turn:valid_dialogue_context(Context0), " +
            "symbolic_dialogue_turn:dialogue_turn(\"$escapedText\", conversation, Context0, " +
            "turn(_Frames, Act, Context1)), " +
            "symbolic_dialogue_turn:valid_dialogue_context(Context1)"
    }

    private fun prologString(raw: String): String = buildString(raw.length + 8) {
        raw.forEach { character ->
            when (character) {
                '\\' -> append("\\\\")
                '"' -> append("\\\"")
                '\n' -> append("\\n")
                '\r' -> append("\\r")
                '\t' -> append("\\t")
                else -> append(character)
            }
        }
    }

    private val PURE_SYMBOLIC_EXPERT_LIMITS = ExpertLimits(
        timeoutMs = 5_000,
        maxResults = 8,
        maxOutputBytes = 64 * 1024,
        maxModelCalls = 0,
    )
    private const val DEFAULT_EXPERT_WORKSPACE = "local-device"
    private const val CANONICAL_EXPERT_QUERY_MARKER = "expert.invoke"
    private const val DIALOGUE_CONTEXT_WIRE_PREFIX = "__zara_context__:"
    private const val DIALOGUE_ACT_WIRE_PREFIX = "__zara_act__:"
    private const val DIALOGUE_EXPERT_EVIDENCE_WIRE_PREFIX = "__zara_expert_evidence__:"
    private const val RUNTIME_FAILURE_TEXT = "The symbolic runtime could not complete this turn."
    private const val NO_MATCH_TEXT = "I don't have a deterministic symbolic answer for that yet."
    private const val MAX_DIALOGUE_ACT_CHARS = 64
    private const val MAX_EXPERT_EVIDENCE_CHARS = 128
    private const val MAX_EXPERT_SUMMARY_CHARS = 1_024
    private const val MAX_UTTERANCE_CHARS = 8_192
}
