:- module(kb_capabilities, [
    capability_provider/3
]).

capability_provider(search, web_search, 100).

capability_provider(open, mapped_app, 100).
capability_provider(open, direct_app, 50).
capability_provider(open, executable_fallback, 10).
