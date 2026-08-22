:- module(kb_capabilities, [
    capability_provider/3,
    capability_property/2
]).

capability_provider(search, web_search, 100).
capability_property(web_search, scope(web)).
capability_property(web_search, transport(network)).
capability_property(web_search, effect(external_process)).
capability_property(web_search, certainty(configured)).

capability_provider(open, mapped_app, 100).
capability_property(mapped_app, scope(local)).
capability_property(mapped_app, transport(process)).
capability_property(mapped_app, effect(external_process)).
capability_property(mapped_app, certainty(configured)).

capability_provider(open, direct_app, 50).
capability_property(direct_app, scope(local)).
capability_property(direct_app, transport(process)).
capability_property(direct_app, effect(external_process)).
capability_property(direct_app, certainty(known)).

capability_provider(open, executable_fallback, 10).
capability_property(executable_fallback, scope(local)).
capability_property(executable_fallback, transport(process)).
capability_property(executable_fallback, effect(external_process)).
capability_property(executable_fallback, certainty(speculative)).
