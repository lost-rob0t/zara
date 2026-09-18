# Zara from First Principles — Book Style Contract

## Purpose

This book teaches the Zara core by building a real personal AI from scratch in Prolog.

The primary implementation path must remain useful with every LLM disabled. Neural models and LLMs are introduced only near the end as optional capabilities/backends.

## Teaching style

1. Build intelligence first; do not begin with frameworks or architecture diagrams.
2. Introduce architecture only when a concrete implementation problem demands it.
3. Teach Prolog concepts through Zara problems, not isolated syntax lessons.
4. Every chapter must leave the reference implementation in a runnable state.
5. Every chapter should add one observable, testable capability.
6. Prefer small working systems that evolve over large pre-built skeletons.
7. Show why the naive implementation fails before introducing the stronger abstraction.
8. Keep explanations practical, direct, curious, and implementation-driven.
9. Treat provenance, explanations, and “why?” as first-class features from the beginning.
10. The reader should understand how the machinery works, not merely invoke it.

## Core architectural laws

1. Every decision is representable as data or a Prolog rule.
2. Every external side effect goes through an explicit capability.
3. Every learned fact has provenance.
4. Every important conclusion can answer “why?”
5. Removing every LLM must leave a useful assistant.

## Chapter contract

Every chapter should contain:

- the concrete problem we are solving;
- the intentionally-naive starting implementation;
- the failure or limitation that motivates the new abstraction;
- the Prolog concept required to solve it;
- the implementation;
- runnable examples;
- tests;
- explanation/provenance behavior where applicable;
- exercises;
- a git tag/checkpoint or equivalent reproducible state.

## Reference implementation pipeline

```text
Input
  ↓
Tokenizer / Parser
  ↓
Semantic term
  ↓
Dialogue manager
  ↓
Goal resolver
  ↓
Planner
  ↓
Capability resolver
  ↓
Tool / Actor
  ↓
Observation
  ↓
Knowledge + Memory
  ↓
Response generator
```

The Prolog runtime owns decision-making. Python, Kotlin, JavaScript, shell, native services, and UI frameworks are adapters at the edges unless a chapter explicitly demonstrates an alternative.

## Narrative progression

The reader should rediscover the architecture:

- direct response predicates become intents;
- intents become semantic terms;
- semantic terms expose the need for provenance;
- direct execution exposes the need for capabilities;
- synchronous capabilities expose the need for actors;
- actors expose discovery, supervision, and lifecycle problems;
- plugin requirements force a real extension system;
- client requirements force a stable protocol;
- only after symbolic Zara is useful do search, embeddings, local models, and LLMs appear.

## Book title

**Zara from First Principles: Build a Personal AI by Hand with Prolog**

The book is simultaneously:

- a Zara core manual;
- a full-stack Prolog course;
- a symbolic-AI textbook;
- a build-your-own-personal-assistant project;
- a clean-room implementation of a small but real Zara clone.


## Relationship to the canonical Zara user book

Issue #911 and `docs/book.org` remain the canonical end-user/operator Zara Book. **Zara from First Principles** is a separate implementation textbook: it teaches Prolog, symbolic AI, and a clean-room teaching clone from the ground up.

The implementation textbook may link to the canonical user book for current shipped behavior, but it must not compete with or silently redefine the user guide's feature-status authority.
