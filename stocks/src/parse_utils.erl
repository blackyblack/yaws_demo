-module(parse_utils).
-include("stocks_info.hrl").

-export([parse_erlang/1, validate_record/1,
         string_to_datetime/1, valid_time/1, scale_to_atom/1]).

%% parses erlang terms in string
%% parse_erlang(S::string()) = [term()]
parse_erlang(S) ->
  SD = lists:append(S, "."),
  {ok, Tokens, _} = erl_scan:string(SD),
  {ok, Terms} = erl_parse:parse_term(Tokens),
  Terms.

%% throw error on bad record.
validate_record(#stock_record{
                  name = Name, datetime = DateTime, 
                  price = Price, count = Count} = R) ->
  true = (is_integer(Price) or is_float(Price)),
  true = is_integer(Count),
  true = is_integer(DateTime),
  true = (string:len(Name) > 0),
  R.
    
%% Parse string with erlang datetime value to datetime term.
%% Fail if DateTimeStr not valid.
%% string_to_datetime(DateTimeStr::string()) = datetime()
string_to_datetime(DateTimeStr) ->
  Terms = parse_erlang(DateTimeStr),
  %% validate
  {Date, Time} = Terms,
  true = calendar:valid_date(Date),
  true = valid_time(Time),
  Terms.

%% valid_time({Hour, Minute, Second}) = true | false
valid_time({H, M, S}) when is_integer(H), is_integer(M), is_integer(S) ->
  valid_time1(H, M, S).

valid_time1(H, M, S) when H >= 0, H < 24, M >= 0, M < 60, S >= 0, S < 60 ->
  true;
valid_time1(_, _, _) ->
  false.

%% allowed scales
%% scale_to_atom(Scale::string()) = scale()
scale_to_atom("month") ->
   month;
scale_to_atom("week") ->
   week;
scale_to_atom("day") ->
   day;
scale_to_atom("hour") ->
   hour;
scale_to_atom("minute") ->
   minute.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

erlang_parse_test() ->
  ?assertEqual([{atom1, "Str"}], parse_erlang("[{atom1, \"Str\"}]")).

valid_test() ->
  R = #stock_record{
        name = "x", datetime = 100, 
        price = 1.0, count = 10},
  ?assertEqual(R, validate_record(R)),
  B = #stock_record{
        name = "x", datetime = 100, 
        price = 1.0, count = abc},
  ?assertError(_, validate_record(B)),
  ?assertEqual({{2000,1,1},{0,0,0}}, 
               string_to_datetime("{{2000,1,1}, {0,0,0}}")),
  ?assertError(_, string_to_datetime("{{2000,13,1}, {0,0,0}}")),
  ?assertError(_, string_to_datetime("{{2000,12,1}, {0,65,0}}")),
  ?assertError(_, string_to_datetime("{{2000,12,1}, {a,0,0}}")),
  ?assertEqual(true, valid_time({1, 30, 55})),
  ?assertEqual(false, valid_time({1, 66, 55})).

-endif.