:- module(market_data,
    [
        market_quote/2,
        market_quote/3,
        market_search/2,
        market_search/3,
        market_daily_bars/3,
        market_daily_bars/4,
        market_provider/1,
        current_market_provider/1,
        set_market_provider/1,
        market_provider_available/1,
        market_provider_quote/3,
        market_provider_search/3,
        market_provider_daily_bars/4,
        market_request_json/3
    ]).

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(uri)).
:- use_module(library(lists)).

:- dynamic selected_market_provider/1.

:- multifile market_provider_available/1.
:- multifile market_provider_quote/3.
:- multifile market_provider_search/3.
:- multifile market_provider_daily_bars/4.

%% market_provider(-Provider) is nondet.
%
%  Enumerate providers with a registered quote implementation. User-owned
%  executable Prolog configuration may extend market_provider_quote/3,
%  market_provider_search/3, and market_provider_daily_bars/4 directly.
market_provider(Provider) :-
    clause(market_provider_quote(Provider, _, _), _),
    nonvar(Provider).

%% current_market_provider(-Provider) is det.
%
%  Resolution order is an explicit runtime override, ZARA_MARKET_PROVIDER,
%  then the built-in Alpha Vantage adapter.
current_market_provider(Provider) :-
    selected_market_provider(Provider), !.
current_market_provider(Provider) :-
    getenv('ZARA_MARKET_PROVIDER', Raw),
    Raw \== '',
    text_atom(Raw, Provider), !.
current_market_provider(alpha_vantage).

%% set_market_provider(+Provider) is det.
%
%  Set the process-local provider. The provider need not be built in; this is
%  intentionally open so config.exec.pl can define a provider and select it.
set_market_provider(Provider) :-
    must_be(atom, Provider),
    retractall(selected_market_provider(_)),
    assertz(selected_market_provider(Provider)).

market_quote(Symbol, Quote) :-
    current_market_provider(Provider),
    market_quote(Provider, Symbol, Quote).

market_quote(Provider, Symbol, Quote) :-
    must_be(atom, Provider),
    normalize_symbol(Symbol, Normalized),
    require_provider_available(Provider),
    once(market_provider_quote(Provider, Normalized, Quote)), !.
market_quote(Provider, _, _) :-
    throw(error(existence_error(market_quote_provider, Provider), _)).

market_search(Query, Matches) :-
    current_market_provider(Provider),
    market_search(Provider, Query, Matches).

market_search(Provider, Query, Matches) :-
    must_be(atom, Provider),
    text_string(Query, Normalized),
    Normalized \== "",
    require_provider_available(Provider),
    once(market_provider_search(Provider, Normalized, Matches)), !.
market_search(Provider, _, _) :-
    throw(error(existence_error(market_search_provider, Provider), _)).

market_daily_bars(Symbol, Limit, Bars) :-
    current_market_provider(Provider),
    market_daily_bars(Provider, Symbol, Limit, Bars).

market_daily_bars(Provider, Symbol, Limit, Bars) :-
    must_be(atom, Provider),
    normalize_symbol(Symbol, Normalized),
    must_be(integer, Limit),
    ( Limit >= 1, Limit =< 5000
    -> true
    ; throw(error(domain_error(market_bar_limit, Limit), _))
    ),
    require_provider_available(Provider),
    once(market_provider_daily_bars(Provider, Normalized, Limit, Bars)), !.
market_daily_bars(Provider, _, _, _) :-
    throw(error(existence_error(market_daily_provider, Provider), _)).

require_provider_available(Provider) :-
    ( market_provider_available(Provider)
    -> true
    ; throw(error(permission_error(use, market_provider, Provider),
                  context(market_data, 'provider is not configured or available')))
    ).

normalize_symbol(Symbol, Normalized) :-
    text_string(Symbol, Text),
    normalize_space(string(Trimmed), Text),
    ( Trimmed == ""
    -> throw(error(domain_error(market_symbol, Symbol), _))
    ; string_upper(Trimmed, Upper),
      atom_string(Normalized, Upper)
    ).

text_atom(Value, Atom) :-
    atom(Value), !, Atom = Value.
text_atom(Value, Atom) :-
    string(Value), !, atom_string(Atom, Value).
text_atom(Value, Atom) :-
    term_string(Value, Text),
    atom_string(Atom, Text).

text_string(Value, Text) :-
    string(Value), !, Text = Value.
text_string(Value, Text) :-
    atom(Value), !, atom_string(Value, Text).
text_string(Value, Text) :-
    number(Value), !, number_string(Value, Text).
text_string(Value, Text) :-
    term_string(Value, Text).

text_number(Value, Number) :-
    number(Value), !, Number = Value.
text_number(Value, Number) :-
    text_string(Value, Text),
    number_string(Number, Text).

percent_number(Value, Number) :-
    text_string(Value, Text),
    ( sub_string(Text, 0, Length, 1, NumberText),
      sub_string(Text, Length, 1, 0, "%")
    -> number_string(Number, NumberText)
    ; number_string(Number, Text)
    ).

dict_text(Dict, Key, Value) :-
    get_dict(Key, Dict, Raw),
    text_string(Raw, Value).

dict_number(Dict, Key, Value) :-
    get_dict(Key, Dict, Raw),
    text_number(Raw, Value).

dict_optional_number(Dict, Key, Value) :-
    ( get_dict(Key, Dict, Raw), Raw \== ""
    -> text_number(Raw, Value)
    ; Value = null
    ).

%% market_request_json(+URL, +TimeoutSeconds, -Reply) is det.
%
%  Shared JSON GET primitive intentionally exported for executable Prolog
%  config and third-party provider adapters.
market_request_json(URL, Timeout, Reply) :-
    text_string(URL, URLText),
    must_be(number, Timeout),
    ( Timeout > 0, Timeout =< 120
    -> true
    ; throw(error(domain_error(market_http_timeout, Timeout), _))
    ),
    catch(
        setup_call_cleanup(
            http_open(URLText, Stream,
                [ status_code(Status),
                  timeout(Timeout),
                  request_header('User-Agent'='Zara/market-data')
                ]),
            json_read_dict(Stream, Reply),
            close(Stream)
        ),
        Error,
        throw(error(market_http_error(Error), _))
    ),
    ( between(200, 299, Status)
    -> true
    ; throw(error(market_http_status(Status, URLText), _))
    ).

% ---------------------------------------------------------------------------
% Alpha Vantage built-in provider
% ---------------------------------------------------------------------------

market_provider_available(alpha_vantage) :-
    alpha_vantage_api_key(_).

market_provider_quote(alpha_vantage, Symbol, Quote) :-
    alpha_vantage_request(
        [function='GLOBAL_QUOTE', symbol=Symbol],
        Reply
    ),
    alpha_vantage_success(Reply),
    normalize_alpha_vantage_quote(Reply, Quote).

market_provider_search(alpha_vantage, Query, Matches) :-
    alpha_vantage_request(
        [function='SYMBOL_SEARCH', keywords=Query],
        Reply
    ),
    alpha_vantage_success(Reply),
    normalize_alpha_vantage_search(Reply, Matches).

market_provider_daily_bars(alpha_vantage, Symbol, Limit, Bars) :-
    alpha_vantage_request(
        [function='TIME_SERIES_DAILY', symbol=Symbol, outputsize=compact],
        Reply
    ),
    alpha_vantage_success(Reply),
    normalize_alpha_vantage_daily(Reply, Limit, Bars).

alpha_vantage_endpoint(Endpoint) :-
    ( getenv('ZARA_ALPHA_VANTAGE_ENDPOINT', Raw), Raw \== ''
    -> text_string(Raw, Endpoint)
    ; Endpoint = "https://www.alphavantage.co/query"
    ).

alpha_vantage_api_key(Key) :-
    getenv('ALPHAVANTAGE_API_KEY', Raw),
    Raw \== '',
    text_string(Raw, Key).

alpha_vantage_timeout(Timeout) :-
    ( getenv('ZARA_MARKET_TIMEOUT', Raw), Raw \== '',
      catch(text_number(Raw, Parsed), _, fail), Parsed > 0, Parsed =< 120
    -> Timeout = Parsed
    ; Timeout = 15
    ).

alpha_vantage_request(Parameters, Reply) :-
    ( alpha_vantage_api_key(Key)
    -> true
    ; throw(error(missing_api_key(alpha_vantage, 'ALPHAVANTAGE_API_KEY'), _))
    ),
    alpha_vantage_endpoint(Endpoint),
    append(Parameters, [apikey=Key], QueryPairs),
    uri_query_components(Query, QueryPairs),
    format(string(URL), '~s?~s', [Endpoint, Query]),
    alpha_vantage_timeout(Timeout),
    market_request_json(URL, Timeout, Reply).

alpha_vantage_success(Reply) :-
    ( get_dict('Error Message', Reply, Message)
    -> throw(error(market_provider_error(alpha_vantage, invalid_request, Message), _))
    ; get_dict('Note', Reply, Note)
    -> throw(error(market_provider_error(alpha_vantage, rate_limit, Note), _))
    ; get_dict('Information', Reply, Info)
    -> throw(error(market_provider_error(alpha_vantage, information, Info), _))
    ; true
    ).

normalize_alpha_vantage_quote(Reply, Quote) :-
    ( get_dict('Global Quote', Reply, Global),
      is_dict(Global),
      get_dict('01. symbol', Global, _)
    -> true
    ; throw(error(market_provider_error(alpha_vantage, malformed_quote, Reply), _))
    ),
    dict_text(Global, '01. symbol', Symbol),
    dict_number(Global, '02. open', Open),
    dict_number(Global, '03. high', High),
    dict_number(Global, '04. low', Low),
    dict_number(Global, '05. price', Price),
    dict_optional_number(Global, '06. volume', Volume),
    dict_text(Global, '07. latest trading day', TradingDay),
    dict_number(Global, '08. previous close', PreviousClose),
    dict_number(Global, '09. change', Change),
    get_dict('10. change percent', Global, PercentRaw),
    percent_number(PercentRaw, ChangePercent),
    Quote = _{
        provider:alpha_vantage,
        symbol:Symbol,
        price:Price,
        open:Open,
        high:High,
        low:Low,
        volume:Volume,
        trading_day:TradingDay,
        previous_close:PreviousClose,
        change:Change,
        change_percent:ChangePercent
    }.

normalize_alpha_vantage_search(Reply, Matches) :-
    ( get_dict(bestMatches, Reply, RawMatches), is_list(RawMatches)
    -> maplist(normalize_alpha_vantage_match, RawMatches, Matches)
    ; throw(error(market_provider_error(alpha_vantage, malformed_search, Reply), _))
    ).

normalize_alpha_vantage_match(Raw, Match) :-
    dict_text(Raw, '1. symbol', Symbol),
    dict_text(Raw, '2. name', Name),
    dict_text(Raw, '3. type', Type),
    dict_text(Raw, '4. region', Region),
    dict_text(Raw, '8. currency', Currency),
    dict_number(Raw, '9. matchScore', Score),
    Match = _{
        provider:alpha_vantage,
        symbol:Symbol,
        name:Name,
        type:Type,
        region:Region,
        currency:Currency,
        score:Score
    }.

normalize_alpha_vantage_daily(Reply, Limit, Bars) :-
    ( get_dict('Time Series (Daily)', Reply, Series), is_dict(Series)
    -> true
    ; throw(error(market_provider_error(alpha_vantage, malformed_daily, Reply), _))
    ),
    dict_pairs(Series, _, Pairs0),
    keysort(Pairs0, Ascending),
    reverse(Ascending, Descending),
    take_prefix(Limit, Descending, Selected),
    maplist(normalize_alpha_vantage_bar, Selected, Bars).

normalize_alpha_vantage_bar(Date-Raw, Bar) :-
    dict_number(Raw, '1. open', Open),
    dict_number(Raw, '2. high', High),
    dict_number(Raw, '3. low', Low),
    dict_number(Raw, '4. close', Close),
    dict_optional_number(Raw, '5. volume', Volume),
    text_string(Date, DateText),
    Bar = _{
        provider:alpha_vantage,
        date:DateText,
        open:Open,
        high:High,
        low:Low,
        close:Close,
        volume:Volume
    }.

take_prefix(0, _, []) :- !.
take_prefix(_, [], []) :- !.
take_prefix(N, [Head|Tail], [Head|Rest]) :-
    N > 0,
    Next is N - 1,
    take_prefix(Next, Tail, Rest).
