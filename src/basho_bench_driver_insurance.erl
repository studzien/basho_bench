-module(basho_bench_driver_insurance).

-export([new/1,
         run/4,
         terminate/2]).

-record(state, {username, password,
                users,
                decision, tickets = [], attachments = []}).
%% ====================================================================
%% API
%% ====================================================================
new(Id) ->
    inets:start(),
    #state{users=Users} = State = prepare_wsdls(),
    Username = username(Id),
    Password = password(Id),
    detergent:call(Users, "register", [Username, "dummy@email.com", Password]),
    {ok, undefined, [{_,_,0}]} = detergent:call(Users, "authorization", [Username, Password]),
    {ok, State#state{username = Username, password = Password}}.

run({user, changeEmail}, _, _, #state{username=Username}=State) ->
    Email = "other@email.com",
    detergent:call(service(users, State), "changeEmail", [Username, Email]),
    {ok, State};
run({user, getUsers}, _, _, State) ->
    detergent:call(service(users, State), "getUsers", []),
    {ok, State};
run({decision, Op}, KeyGen, ValueGen, State) ->
    run_decision(Op, KeyGen, ValueGen, State);
run(Op, _KeyGen, _ValueGen, State) ->
    error_logger:info_msg("~p~n", [Op]),
    {silent, State}.

terminate(_Reason, #state{username = Username}=State) ->
    detergent:call(service(users,State), "removeUser", [Username]),
    [scrub:call(service(decision,State), "deleteTicket", [Id]) ||
        Id <- State#state.tickets].

%% ====================================================================
%% Particular service handlers
%% ====================================================================
run_decision(addTicket, _, _, #state{tickets=Data}=State) ->
    Description = random_string(140),
    case scrub:call(service(decision, State), "addTicket",
                    [Description, "creator"]) of
        {ok, _, [{_, _, Id}]} ->
            {ok, State#state{tickets=[Id|Data]}};
        _ ->
            {error, bad_response, State}
    end;
run_decision(listTickets, _, _, State) ->
    scrub:call(service(decision, State), "listTickets", ["owner"]),
    {ok, State};
run_decision(getTickets, _, _, State) ->
    scrub:call(service(decision, State), "getTickets", ["creator"]),
    {ok, State};
run_decision(getAllTickets, _, _, State) ->
    scrub:call(service(decision, State), "getAllTickets", []),
    {ok, State};
run_decision(deleteTicket, _, _, #state{tickets=[Id|Rest]}=State) ->
    scrub:call(service(decision, State), "deleteTicket", [Id]),
    {ok, State#state{tickets=Rest}};
run_decision(takeOwnership, _, _, #state{tickets=[Id|_]}=State) ->
    scrub:call(service(decision, State), "takeOwnership", [Id, "owner"]),
    {ok, State};
run_decision(requestEval, _, _, #state{tickets=[Id|_]}=State) ->
    scrub:call(service(decision, State), "requestEval",
               [Id, random_string(255)]),
    {ok, State};
run_decision(getTicketHistory, _, _, #state{tickets=[Id|_]}=State) ->
    scrub:call(service(decision, State), "getTicketHistory", [Id]),
    {ok, State};
run_decision(getTicket, _, _, #state{tickets=[Id|_]}=State) ->
    scrub:call(service(decision, State), "getTicket", [Id]),
    {ok, State};
run_decision(getAttachment, _, _,
             #state{attachments=[{Ticket,Att}|_]}=State) ->
    scrub:call(service(decision, State), "getAttachment", [Ticket, Att]),
    {ok, State};
run_decision(evalEta, _, _, #state{tickets=[Id|_]}=State) ->
    scrub:call(service(decision, State), "evalEta",
               [Id, iso_8601_fmt(calendar:now_to_local_time(now()))]),
    {ok, State};
run_decision(deleteDoc, _, _,
             #state{attachments=[{Ticket,Att}|Rest]}=State) ->
    scrub:call(service(decision, State), "deleteDoc", [Ticket, Att]),
    {ok, State#state{attachments=Rest}};
run_decision(attachDoc, _, ValueGen,
             #state{tickets=[Id|_],attachments=Atts}=State) ->
    Name = random_string(20),
    scrub:call(service(decision, State), "attachDoc",
               [Id, Name, "image/bmp", base64:encode_to_string(ValueGen())]),
    {ok, State#state{attachments=[{Id, Name}|Atts]}};
run_decision(addDecission, _, _, #state{tickets=[Id|_]}=State) ->
    Decision = random_string(6),
    scrub:call(service(decision, State), "addDecission",
               [Id, Decision, "decidor"]),
    {ok, State};
run_decision(_Op, _, _, State) ->
    {silent, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
random_string(Length) ->
    [random:uniform(93)+33 || _ <- lists:seq(1,Length)].

username(Id) -> "benchmarkuser_" ++ integer_to_list(Id).
password(Id) -> "password" ++ integer_to_list(Id).

service(users, State) ->
    State#state.users;
service(decision, State) ->
    State#state.decision.

prepare_wsdls() ->
    UsersEndpoint = basho_bench_config:get(users_endpoint),
    UsersWsdl = detergent:initModel(UsersEndpoint),
    DecisionEndpoint = basho_bench_config:get(decision_endpoint),
    DecisionWsdl = scrub:initModel(DecisionEndpoint),
    #state{users = UsersWsdl, decision=DecisionWsdl}.

iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
                                [Year, Month, Day, Hour, Min, Sec])).
