:- begin_tests(market_data).

:- use_module('../modules/market_data').

:- multifile market_data:market_provider_available/1.
:- multifile market_data:market_provider_quote/3.
:- multifile market_data:market_provider_search/3.
:- multifile market_data:market_provider_daily_bars/4.

market_data:market_provider_available(test_feed).
market_data:market_provider_quote(test_feed, Symbol, _{
    provider:test_feed,
    symbol:Symbol,
    price:42.5
}).
market_data:market_provider_search(test_feed, Query, [_{
    provider:test_feed,
    symbol:'TEST',
    name:Query
}]).
market_data:market_provider_daily_bars(test_feed, Symbol, Limit, [_{
    provider:test_feed,
    symbol:Symbol,
    limit:Limit,
    close:42.5
}]).

clear_selected_provider :-
    retractall(market_data:selected_market_provider(_)).

test(custom_provider_quote_normalizes_symbol) :-
    market_data:market_quote(test_feed, " aapl ", Quote),
    Quote.provider == test_feed,
    Quote.symbol == 'AAPL',
    Quote.price =:= 42.5.

test(custom_provider_can_be_selected,
     [setup(market_data:set_market_provider(test_feed)),
      cleanup(clear_selected_provider)]) :-
    market_data:current_market_provider(test_feed),
    market_data:market_quote(msft, Quote),
    Quote.symbol == 'MSFT'.

test(custom_provider_search) :-
    market_data:market_search(test_feed, "Acme Corp", Matches),
    Matches = [Match],
    Match.symbol == 'TEST',
    Match.name == "Acme Corp".

test(custom_provider_daily_bars) :-
    market_data:market_daily_bars(test_feed, nvda, 10, Bars),
    Bars = [Bar],
    Bar.symbol == 'NVDA',
    Bar.limit =:= 10.

test(daily_bar_limit_is_bounded,
     [throws(error(domain_error(market_bar_limit, 101), _))]) :-
    market_data:market_daily_bars(test_feed, aapl, 101, _).

test(alpha_vantage_quote_normalization) :-
    Reply = _{
        'Global Quote': _{
            '01. symbol': "AAPL",
            '02. open': "225.00",
            '03. high': "230.00",
            '04. low': "224.00",
            '05. price': "228.50",
            '06. volume': "1234567",
            '07. latest trading day': "2026-09-16",
            '08. previous close': "226.00",
            '09. change': "2.50",
            '10. change percent': "1.1062%"
        }
    },
    market_data:normalize_alpha_vantage_quote(Reply, Quote),
    Quote.symbol == "AAPL",
    Quote.price =:= 228.50,
    Quote.volume =:= 1234567,
    Quote.change_percent =:= 1.1062.

test(alpha_vantage_search_normalization) :-
    Reply = _{
        bestMatches: [_{
            '1. symbol': "IBM",
            '2. name': "International Business Machines",
            '3. type': "Equity",
            '4. region': "United States",
            '8. currency': "USD",
            '9. matchScore': "1.0000"
        }]
    },
    market_data:normalize_alpha_vantage_search(Reply, Matches),
    Matches = [Match],
    Match.symbol == "IBM",
    Match.currency == "USD",
    Match.score =:= 1.0.

test(alpha_vantage_daily_is_newest_first_and_limited) :-
    Reply = _{
        'Time Series (Daily)': _{
            '2026-09-14': _{
                '1. open': "10",
                '2. high': "12",
                '3. low': "9",
                '4. close': "11",
                '5. volume': "100"
            },
            '2026-09-16': _{
                '1. open': "12",
                '2. high': "14",
                '3. low': "11",
                '4. close': "13",
                '5. volume': "300"
            },
            '2026-09-15': _{
                '1. open': "11",
                '2. high': "13",
                '3. low': "10",
                '4. close': "12",
                '5. volume': "200"
            }
        }
    },
    market_data:normalize_alpha_vantage_daily(Reply, 2, Bars),
    Bars = [Newest, Previous],
    Newest.date == "2026-09-16",
    Newest.close =:= 13,
    Previous.date == "2026-09-15",
    Previous.close =:= 12.

:- end_tests(market_data).
