:- module(desktop_design_expert,
    [ expert_id/1,
      provider_policy/1,
      max_model_calls/1,
      desktop_role_source/3,
      desktop_role_derivation/3,
      theme_provenance/2
    ]).

:- use_module('../kb/desktop_design').

expert_id('zara:expert/desktop-design').
provider_policy(disabled).
max_model_calls(0).

desktop_role_source(Theme, DesktopRole, AndroidToken) :-
    desktop_role_derivation(Theme, DesktopRole, Rule),
    rule_source_token(Rule, AndroidToken).

desktop_role_derivation(Theme, DesktopRole, Rule) :-
    kb_desktop_design:supported_theme(Theme),
    kb_desktop_design:desktop_role_rule(DesktopRole, Rule).

theme_provenance(Theme, Source) :-
    kb_desktop_design:design_theme_provenance(Theme, Source).

rule_source_token(direct(Token), Token).
rule_source_token(contrast_text(Token), Token).
rule_source_token(mix(Token, _, _), Token).
