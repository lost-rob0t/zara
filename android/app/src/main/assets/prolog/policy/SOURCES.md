# Policy KB provenance

The default facts are copied without modification from `lost-rob0t/zara-plugins` commit `59b71925f5aa866dc8954eca10f95b839f78a98e`, `plugins/zara-policy/lib/zara_policy/prolog/defaults.pl`, Git blob `2ffb6c65558e004080203c1dafb74e41208165f0`.

This is the English starter KB: 75 project-authored rules and 291 phrase alternatives. The research supports failure families, not these exact strings or any claimed classifier accuracy. Matches request contextual review; no match does not establish truth or task completion. Style rules are disabled by default.

Source IDs retained from the upstream plugin:

- `sycophancy`: Anthropic, *Towards Understanding Sycophancy in Language Models* (2023), https://www.anthropic.com/research/towards-understanding-sycophancy-in-language-models
- `hallucinations`: OpenAI, *Why language models hallucinate* (2025), https://openai.com/index/why-language-models-hallucinate/
- `self_refine`: Madaan et al., *Self-Refine: Iterative Refinement with Self-Feedback* (2023), https://arxiv.org/abs/2303.17651
- `self_correction`: Huang et al., *Large Language Models Cannot Self-Correct Reasoning Yet* (2023), https://arxiv.org/abs/2310.01798
- `self_debug`: Chen et al., *Teaching Large Language Models to Self-Debug* (2023), https://arxiv.org/abs/2304.05128
- `xstest`: Rottger et al., *XSTest: A Test Suite for Identifying Exaggerated Safety Behaviours in Large Language Models* (2023), https://arxiv.org/abs/2308.01263
- `engineering`: original Zara policy design, zara-plugins issue #811; not a research-validated detector.
- `local`: operator-authored extensions and their supplied provenance.

The Android adapter uses numeric Prolog terms, not the desktop JSON/subprocess transport. Its native gate executes this snapshot under both SWI-Prolog and the pinned Trealla runtime, then exercises the actual JNI C bridge. These checks do not imply physical-device verification.
