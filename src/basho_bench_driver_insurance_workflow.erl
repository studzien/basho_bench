-module(basho_bench_driver_insurance_workflow).

-export([run/4]).

-include("include/basho_bench_driver_insurance.hrl").
-include_lib("exml/include/exml.hrl").

run({login, Login, Password}, _, _, #state{sids=Sids}=State) ->
    case send_request(req_login(Login, Password), State) of
        {ok, Response} ->
            {ok, State#state{sids = [get_sid(Response)|Sids]}};
        {error, Reason} ->
            {error, Reason, State}
    end;
run(logout, _, _, #state{sids=[Sid|Rest]}=State) ->
    case send_request(req_logout(Sid), State) of
        {ok, _Response} ->
            {ok, State#state{sids = Rest}};
        {error, Reason} ->
            {error, Reason, State}
    end;
run(_Op, _KeyGen, _ValueGen, State) ->
    {silent, State}.

req_login(Login, Password) ->
    Body = #xmlel{name = <<"ws:login">>,
                  children = [el(<<"login">>, Login),
                              el(<<"password">>, Password)]},
    envelope(Body).

req_logout(Sid) ->
    Body = #xmlel{name = <<"ws:logout">>,
                  children = [el(<<"sid">>, Sid)]},
    envelope(Body).

el(Name, Value) ->
    #xmlel{name = Name, children = #xmlcdata{content = Value}}.

envelope(Body) ->
    #xmlel{name = <<"soapenv:Envelope">>,
           attrs = [{<<"xmlns:soapenv">>,
                     <<"http://schemas.xmlsoap.org/soap/envelope/">>},
                    {<<"xmlns:ws">>,
                     <<"http://service.dms.pl/ws">>}],
           children = [#xmlel{name = <<"soapenv:Header">>},
                       #xmlel{name = <<"soapenv:Body">>,
                              children = [Body]}
                      ]}.

send_request(Request, #state{workflow=Endpoint}) ->
    case lhttpc:request(Endpoint, "POST", [], exml:to_binary(Request), 60000) of
        {ok, {{200, "OK"}, _, Response}} ->
            exml:parse(Response);
        {ok, {{_, Status}, _, _}} ->
            {error, Status};
        Other ->
            Other
    end.

get_sid(Response) ->
    exml_query:path(Response,
                    [{element, <<"soap:Body">>},
                     {element, <<"ns1:loginResponse">>},
                     {element, <<"AuthenticationResponse">>}, 
                     {element, <<"sid">>}, 
                     cdata]).
