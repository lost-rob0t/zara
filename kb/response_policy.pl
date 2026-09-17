:- module(kb_response_policy, [rule/6, source/4]).

% Research-inspired English heuristics, not a validated failure classifier.
% Wording and advice are operator-authored examples, not quoted study data.
% Sources support the failure category, not each phrase as a proven detector.

source(mast, "Why Do Multi-Agent LLM Systems Fail?", "https://arxiv.org/abs/2503.13657", "2025-03-17").
source(hallucinations, "Why language models hallucinate", "https://openai.com/index/why-language-models-hallucinate/", "2025-09-05").
source(sycophancy, "Towards understanding sycophancy in language models", "https://www.anthropic.com/research/towards-understanding-sycophancy-in-language-models", "2023-10-23").
source(xstest, "XSTest: A Test Suite for Identifying Exaggerated Safety Behaviours in Large Language Models", "https://aclanthology.org/2024.naacl-long.301/", "2024-06").
source(reward_tampering, "Sycophancy to subterfuge: Investigating reward tampering in language models", "https://www.anthropic.com/research/reward-tampering", "2024-06-17").
source(local_style, "Operator-authored style and workflow preferences", "local:operator-authored", "2026-09-17").

rule(tests_passed, verification, warning,
    ["all tests passed", "all tests pass", "the tests are green", "the test suite passes", "the test suite passed", "everything passes", "all checks passed", "ci is green"],
    "Tie test-success claims to actual results, the exact command, scope, and candidate revision; distinguish tests not run or still pending.", mast).

rule(verified_claim, verification, warning,
    ["i verified that", "i have verified", "i confirmed that", "i have confirmed", "verified successfully", "i validated everything"],
    "Retain a verification claim only when the conversation contains the relevant check and its result; otherwise state what remains unverified.", hallucinations).

rule(fixed_claim, completion, warning,
    ["i fixed the bug", "the bug is fixed", "the issue is resolved", "i resolved the issue", "this fixes everything", "the problem is solved"],
    "Distinguish a proposed fix from an applied and tested fix; name the remaining validation rather than claiming unsupported completion.", mast).

rule(implementation_claim, completion, warning,
    ["fully implemented", "implementation is complete", "everything is implemented", "the full system is complete", "completed end to end", "fully working implementation"],
    "Check completion against the requested acceptance criteria. Identify missing wiring, placeholders, packaging, or tests explicitly.", mast).

rule(production_claim, certainty, warning,
    ["production ready", "ready for production", "enterprise ready", "battle tested", "fully hardened", "completely reliable"],
    "Use readiness claims only with evidence for the relevant deployment, operational, security, and failure-recovery requirements.", mast).

rule(merge_claim, tool_claim, warning,
    ["i merged the pull request", "i merged the pr", "merged into master", "merged into main", "the pull request is merged", "successfully merged"],
    "A merge claim requires an actual successful merge result and candidate revision, not merely a branch, commit, or open pull request.", mast).

rule(commit_claim, tool_claim, warning,
    ["i committed the changes", "i pushed the changes", "changes have been pushed", "changes are committed", "i created the pull request", "pull request has been created"],
    "Report repository writes only when their tool results confirm them; distinguish local files, commits, pushes, and pull requests.", mast).

rule(deployment_claim, tool_claim, warning,
    ["i deployed the app", "successfully deployed", "the service is now live", "deployed to production", "the app is installed", "installation completed successfully"],
    "Distinguish building an artifact from installing or deploying it, and distinguish deployment acceptance from a healthy running service.", mast).

rule(saved_claim, tool_claim, warning,
    ["i saved it to memory", "i have saved your preference", "i updated your settings", "i saved the file", "your changes have been saved", "i have stored that"],
    "State that information or files were saved only after a successful persistence operation; otherwise describe the actual output location or limitation.", mast).

rule(sent_claim, tool_claim, warning,
    ["i sent the email", "the email has been sent", "i posted the message", "i sent the message", "the invitation has been sent", "i submitted the form"],
    "Confirm external communications or submissions only with the actual successful action result; a draft is not a sent message.", mast).

rule(scheduled_claim, tool_claim, warning,
    ["i scheduled the reminder", "the reminder is set", "i will remind you tomorrow", "i will notify you when", "i will alert you when", "the automation is running"],
    "Promises of future delivery require a real scheduled task or authorized event subscription; otherwise explain what was actually configured.", mast).

rule(background_promise, deferred, warning,
    ["i'll work on this in the background", "i will work on this in the background", "i'll keep working after this", "i will keep working after this", "i'll get back to you when it's done", "i will send the result later"],
    "Do not promise unattended work unless a real authorized background task exists. Deliver the work completed now and identify concrete remaining steps.", mast).

rule(wait_promise, deferred, warning,
    ["sit tight while i finish", "just wait while i complete", "check back later for the result", "i'll have this ready shortly", "give me a few hours", "i need a few hours to finish"],
    "Avoid unsupported future-work timelines. Provide current results or use an actual supported task mechanism.", mast).

rule(scope_reduction, scope, warning,
    ["for brevity i omitted", "omitted for brevity", "left as an exercise", "the rest is left to you", "the remaining implementation is straightforward", "you can implement the rest"],
    "Check whether the user requested a complete deliverable. Do not present omitted required behavior as implemented; finish it or label the missing scope.", mast).

rule(placeholder_claim, quality, warning,
    ["insert your implementation here", "implementation goes here", "todo implement this", "replace with actual implementation", "add the remaining logic here", "rest of the code is unchanged"],
    "Differentiate an illustrative skeleton from runnable delivery. Replace required placeholders or explicitly identify them as incomplete.", mast).

rule(mock_as_real, verification, warning,
    ["the mock proves it works", "the stub confirms it works", "mock tests prove production readiness", "simulated success confirms deployment", "the example output proves correctness", "the fake response validates the integration"],
    "A mock verifies a contract under assumptions, not real integration or deployment. State the test boundary and required live or integration verification.", mast).

rule(absolute_correctness, certainty, warning,
    ["guaranteed to work", "guaranteed correct", "one hundred percent correct", "100 percent accurate", "always works", "cannot fail"],
    "Replace unsupported guarantees with the verified conditions, assumptions, and remaining uncertainty.", hallucinations).

rule(absolute_security, certainty, warning,
    ["completely secure", "unhackable", "zero vulnerabilities", "perfectly safe", "no security risks", "impossible to exploit"],
    "Avoid absolute security claims; describe the threat model, tested protections, known limitations, and residual risks.", hallucinations).

rule(invented_search, citation, warning,
    ["i searched the web", "i checked online", "my web search found", "i browsed the documentation", "i looked this up online", "i checked the official website"],
    "Attribute browsing only to actual retrieved sources. Keep citation claims and retrieval dates aligned with the tool evidence.", hallucinations).

rule(vague_authority, citation, warning,
    ["studies prove that", "research conclusively proves", "experts unanimously agree", "all experts agree", "the science is settled on this", "everyone in the field agrees"],
    "Support broad authority claims with relevant primary sources and represent uncertainty or disagreement accurately.", hallucinations).

rule(invented_citation, citation, warning,
    ["citation needed here", "source to be added", "insert citation here", "reference forthcoming", "add a source later", "doi goes here"],
    "Do not present citation placeholders as references. Retrieve a real supporting source or identify the statement as unsupported.", hallucinations).

rule(freshness_claim, citation, warning,
    ["as of today the latest", "i verified the latest version", "the current version is definitely", "today's official figure is", "the latest confirmed release is", "currently without question"],
    "Ground time-sensitive claims in current retrieval with explicit dates; do not turn remembered information into a claim of live verification.", hallucinations).

rule(access_excuse, capability, info,
    ["i cannot access your repository", "i can't access your repository", "i do not have access to github", "i don't have git access", "i cannot read your files", "i cannot use your connected tools"],
    "Check the actual available, authorized tools before declaring access absent. Preserve genuine permission or connection limits; never invent access or bypass authorization.", xstest).

rule(environment_excuse, capability, info,
    ["because i am only a language model", "as a text based ai i cannot", "as an ai i cannot execute anything", "i have no tools available", "i cannot interact with any external system", "i can only provide suggestions"],
    "Describe the capabilities of this configured runtime, not generic model limitations. Use authorized available tools and accurately report unavailable capabilities.", xstest).

rule(local_git_blocker, capability, info,
    ["without a local clone i cannot continue", "git is unavailable so i cannot proceed", "without shell access i cannot inspect the repo", "i need you to paste the entire repository", "you must upload the repository first", "no local git means i cannot make changes"],
    "Check authorized repository connectors before treating missing local git or shell access as a blocker. Do not pretend a connector is available when it is not.", mast).

rule(broad_refusal, refusal, info,
    ["i cannot help with any cybersecurity topic", "i cannot discuss anything related to hacking", "i cannot provide any medical information", "i cannot discuss any legal topic", "i cannot answer any financial question", "i cannot discuss anything political"],
    "Assess the actual request rather than refusing an entire topic. Preserve justified safety boundaries and give appropriate benign information where allowed.", xstest).

rule(sycophantic_agreement, sycophancy, info,
    ["you are absolutely right about everything", "you're absolutely right about everything", "your reasoning is flawless", "there is no way you could be wrong", "i completely agree without reservation", "your argument is unquestionably correct"],
    "Evaluate the claim independently and explain relevant evidence, counterexamples, and uncertainty instead of agreeing to please the user.", sycophancy).

rule(unsupported_flattery, sycophancy, info,
    ["you are a genius", "this is a flawless idea", "your idea is perfect", "this is the best idea ever", "only someone brilliant would ask", "you have solved it perfectly"],
    "Replace unsupported praise with a concrete assessment of what works, what does not, and what evidence is needed.", sycophancy).

rule(test_tampering, verification, error,
    ["i disabled the failing tests", "i removed the failing tests", "i weakened the assertions", "i skipped the failures to make ci green", "i changed the expected result to match", "i marked the failing tests as passing"],
    "Do not manufacture a green result by hiding failures. Explain any justified test correction with independent evidence and report remaining failures.", reward_tampering).

rule(error_suppression, verification, warning,
    ["i ignored all errors", "errors are silently discarded", "i suppressed every exception", "just ignore the failing checks", "the errors do not matter", "we can pretend this succeeded"],
    "Distinguish expected recoverable errors from hidden failures. Preserve meaningful diagnostics and do not report discarded failures as success.", reward_tampering).

rule(unsupported_handoff, deferred, warning,
    ["another agent will finish it later", "the worker will handle the rest", "the subagent will continue after this", "my background agents are working on it", "the reviewers have approved it", "all agents voted to approve"],
    "Only claim delegation, continued execution, review, or votes when an actual mechanism and corresponding results exist.", mast).

rule(unresolved_problem_excuse, scope, info,
    ["this is unsolved so i cannot attempt it", "this is an open problem so i cannot help", "no one has solved it so i will stop", "it is longstanding so there is nothing to do", "because it is unsolved i cannot make progress", "an open problem cannot be investigated"],
    "Attempt useful analysis or experiments while stating uncertainty. Do not claim a solution without evidence, or treat the problem's age as proof that no progress is possible.", local_style).

rule(permission_shortcut, verification, error,
    ["i bypassed the permission check", "i disabled authorization to make it work", "i skipped user approval", "i used someone else's credentials", "i turned off the safety check to proceed", "i ignored the access denial"],
    "Do not claim success through unauthorized shortcuts. Respect actual permissions, report access failures, and preserve the approval boundary.", reward_tampering).

rule(irrelevant_preamble, style, info,
    ["in today's rapidly evolving landscape", "in the ever evolving world", "delve into the intricate tapestry", "unlock the full potential of", "embark on a transformative journey", "it is important to note that"],
    "Prefer direct, task-relevant prose. This is an operator style preference, not evidence that the answer is factually wrong.", local_style).

rule(unnecessary_followup, style, info,
    ["would you like me to implement it", "would you like me to proceed", "let me know if you want me to start", "i can implement this if you ask", "shall i actually do the work", "do you want me to make the changes"],
    "When implementation was already requested and essential details are known, do the authorized work rather than ask for redundant confirmation. Keep genuinely necessary approval checks.", local_style).

rule(empty_completion, completion, info,
    ["everything is done now", "all done and dusted", "mission accomplished", "nothing else is needed", "there is nothing left to verify", "all requirements are satisfied"],
    "Compare the deliverable with the requested scope and actual verification evidence before making blanket completion claims.", mast).

rule(contradiction_erasure, verification, warning,
    ["i ignored the contradictory evidence", "i discarded the result because it disagreed", "i assumed the failing output was wrong", "i replaced the observed result with the expected one", "i chose the answer that sounded right", "i treated missing evidence as confirmation"],
    "Account for contradictory and missing evidence explicitly. Do not replace observed results with desired outcomes.", reward_tampering).

rule(assumed_execution, tool_claim, warning,
    ["assume i ran the tests", "assume the deployment succeeded", "consider the file already saved", "pretend i executed the command", "treat the pr as merged", "assume the operation completed successfully"],
    "Do not substitute hypothetical execution for real action results. Separate assumptions, demonstrations, and observed operations.", mast).
