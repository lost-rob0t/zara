# LLM-authored Prolog Wear complications

Zara Android can generate a Wear OS complication as a **facts-only Prolog template**, validate it on the phone, save the source in the normal Prolog workspace, and copy a typed active-template payload to the paired watch through Wear Data Layer.

The generated Prolog is data. It is **not executed on the watch** and it cannot add arbitrary Kotlin, shell commands, URLs, directives, or predicates.

## End-to-end flow

1. Configure a cloud model in **Model Providers**.
2. Open **Prolog Complications**.
3. Describe the complication in natural language.
4. Tap **Generate Prolog with selected LLM**.
5. Zara asks the selected provider for a facts-only template using the coding-purpose lane.
6. Zara validates the returned Prolog against the bounded template ABI.
7. Review or edit the facts.
8. Tap **Save + copy active template to watch**.
9. Zara saves `complication_<id>.pl` in the private Prolog workspace and writes the compiled payload at `/zara/complications/v1/active` in Wear Data Layer.
10. The Wear app validates the payload again, stores it, and requests updates for every active Zara Prolog complication slot.

Data Items persist, so a disconnected watch receives the active template when it reconnects.

## Template ABI

Exactly one template is allowed per source. `complication_template/2` must be first.

```prolog
complication_template(zara_status, short_text).
complication_text(zara_status, "Ready").
complication_title(zara_status, "Zara").
complication_description(zara_status, "Assistant status").
```

Allowed predicates:

```prolog
complication_template(Id, short_text).
complication_template(Id, long_text).
complication_template(Id, ranged_value).

complication_text(Id, "Text").
complication_title(Id, "Optional title").
complication_description(Id, "Accessibility description").
complication_range(Id, Value, Min, Max).
```

`complication_range/4` is required for `ranged_value` and rejected for the text-only types.

Example gauge:

```prolog
complication_template(runtime_load, ranged_value).
complication_text(runtime_load, "72%").
complication_title(runtime_load, "Load").
complication_description(runtime_load, "Runtime load").
complication_range(runtime_load, 72, 0, 100).
```

## Fail-closed validation

The compiler rejects:

- Prolog rules (`:-`) and directives
- unknown predicates
- more than one template declaration
- facts referring to a different template ID
- missing required text
- non-finite or out-of-range gauge values
- oversized source/text/title/description fields
- invalid identifiers
- control characters

Unknown facts are errors, not ignored extensions. This makes LLM output deterministic to audit.

## LLM providers

Template generation uses `CloudModelPurpose.CODING`. That means:

- generic OpenAI-compatible providers work
- OpenRouter works
- `llm.starintel.actor` works when it implements the OpenAI-compatible chat-completions contract
- the Z.AI Coding Plan profile is eligible because the request is explicitly generation of Prolog code

The selected provider's API key remains in Android Keystore-backed storage and is never inserted into the complication source or Data Layer payload.

## Wear rendering

The Wear app exposes `Zara Prolog` as a complication data source supporting:

- `SHORT_TEXT`
- `LONG_TEXT`
- `RANGED_VALUE`

The active Prolog type must match the complication type requested by the watch-face slot. If it does not match, the data source returns `NoDataComplicationData` rather than coercing the template into a different semantic type.

The complication tap action opens the Zara Wear app. Generated templates cannot replace that action.

The provider uses push updates (`UPDATE_PERIOD_SECONDS=0`). A Data Layer update asks Wear OS to refresh all active Zara Prolog complication instances immediately.

## Security boundary

The phone sends only these versioned fields:

- schema version
- revision timestamp
- template id and type
- text, optional title, accessibility description
- optional range value/min/max

There are no executable fields, intents, URLs, file paths, API keys, or arbitrary serialized objects in the wire format.
