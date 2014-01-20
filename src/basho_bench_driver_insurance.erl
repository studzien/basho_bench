-module(basho_bench_driver_insurance).

-export([new/1,
         run/4,
         terminate/2]).

-record(state, {username, password,
                users,
                admin,
                role, roles = [],
                param, params = [],
                damage, damages = [],
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

run({user, Op}, KeyGen, ValueGen, State) ->
    check_return(run_user(Op, KeyGen, ValueGen, State));
run({role, Op}, KeyGen, ValueGen, State) ->
    check_return(run_role(Op, KeyGen, ValueGen, State));
run({param, Op}, KeyGen, ValueGen, State) ->
    check_return(run_param(Op, KeyGen, ValueGen, State));
run({damage, Op}, KeyGen, ValueGen, State) ->
    check_return(run_damage(Op, KeyGen, ValueGen, State));
run({decision, Op}, KeyGen, ValueGen, State) ->
    check_return(run_decision(Op, KeyGen, ValueGen, State));
run({admin, Op}, KeyGen, ValueGen, State) ->
    check_return(run_admin(Op, KeyGen, ValueGen, State));
run(Op, _KeyGen, _ValueGen, State) ->
    error_logger:info_msg("~p~n", [Op]),
    {silent, State}.

terminate(_Reason, #state{username = Username}=State) ->
    detergent:call(service(users,State), "removeUser", [Username]),
    [detergent:call(service(roles,State), "removeRole", [Role]) ||
        Role <- State#state.roles],
    [scrub:call(service(decision,State), "deleteTicket", [Id]) ||
        Id <- State#state.tickets].

%% ====================================================================
%% Particular service handlers
%% ====================================================================
check_return({{ok, _, _}, State}) ->
    {ok, State};
check_return({{error, Message}, State}) ->
    {error, Message, State};
check_return({silent, State}) ->
    {silent, State}.

run_user(changeEmail, _, _, #state{username=Username}=State) ->
    Email = "other@email.com",
    Return = detergent:call(service(users, State), "changeEmail",
                            [Username, Email]),
    {Return, State};
run_user(getUsers, _, _, State) ->
    Return = detergent:call(service(users, State), "getUsers", []),
    {Return, State}.

run_role(addRole, _, _, #state{username=Username, roles=[Role|_]}=State) ->
    Return = detergent:call(service(roles, State), "addRole",
                           [Username, Role]), 
    {Return, State};
run_role(createRole, _, _, #state{roles=Roles}=State) ->
    Role = random_string(10),
    Return = detergent:call(service(roles, State), "createRole", [Role]),
    {Return, State#state{roles=[Role|Roles]}};
run_role(getAllRoles, _, _, State) ->
    Return = detergent:call(service(roles, State), "getAllRoles", []),
    {Return, State};
run_role(getUserRole, _, _, #state{username=Username}=State) ->
    Return = detergent:call(service(roles, State), "getUserRole", [Username]),
    {Return, State};
run_role(removeRole, _, _, #state{roles=[Role|Rest]}=State) ->
    Return = detergent:call(service(roles, State), "removeRole", [Role]),
    {Return, State#state{roles=Rest}};
run_role(revokeRole, _, _, #state{username=Username, roles=[Role|_]}=State) ->
    Return = detergent:call(service(roles, State), "revokeRole",
                            [Username, Role]),
    {Return, State};
run_role(_Op, _, _, State) ->
    {silent, State}.

run_param(getUserParam, _, _, #state{username=Username, params=[Param|_]}=State) ->
    Return = detergent:call(service(params, State), "getUserParam",
                            [Username, Param]),
    {Return, State};
run_param(setUserParam, _, _, #state{username=Username, param=Params}=State) ->
    Key = random_string(20),
    Value = random_string(100),
    Return = detergent:call(service(params, State), "setUserParam",
                            [Username, Key, Value]),
    {Return, State#state{params=[Key|Params]}};
run_param(_Op, _, _, State) ->
    {silent, State}.

run_damage(reportInformation, _, _, #state{damages=Damages}=State) ->
    case detergent:call(service(damages, State), "ReportInformation",
                        [random_string(15) || _ <- lists:seq(1,3)]) of
        {ok, _, [{_, _, Id}]}=Reply ->
            {Reply, State#state{damages=[Id|Damages]}};
        Other ->
            {Other, State}
    end;
run_damage(damageInformation, _, _, #state{damages=[Id|_]}=State) ->
    R = random_string(15),
    Return = detergent:call(service(damages, State), "DamageInformation",
                            [Id, "2013-01-20", R, R, R, true]),
    {Return, State};
run_damage(personReportingDamageData, _, _, #state{damages=[Id|_]}=State) ->
    R = random_string(15),
    Return = detergent:call(service(damages, State), "PersonReportingDamageData",
                            [Id,R,R,"2312","mail@mail.com",R,"13212",R,R,R,R]),
    {Return, State};
run_damage(ownerDamagedVehicleData, _, _, #state{damages=[Id|_]}=State) ->
    Return = detergent:call(service(damages, State), "OwnerDamagedVehicleData",
                            [Id, 2, "01234567890"]),
    {Return, State};
run_damage(damagedVehicleData, _, _, #state{damages=[Id|_]}=State) ->
    R = random_string(20),
    Return = detergent:call(service(damages, State), "DamagedVehicleData",
                            [Id,R,R,R,2000,R,"2013-01-20","2013-01-20","2013-01-20"]),
    {Return, State};
run_damage(confirmation, _, _, #state{damages=[Id|_]}=State) ->
    Return = detergent:call(service(damages, State), "Confirmation", [Id]),
    {Return, State};
run_damage(_Op, _, _, State) ->
    {silent, State}.

run_decision(addTicket, _, _, #state{tickets=Data}=State) ->
    Description = random_string(140),
    case scrub:call(service(decision, State), "addTicket",
                    [Description, "creator"]) of
        {ok, _, [{_, _, Id}]}=Reply ->
            {Reply, State#state{tickets=[Id|Data]}};
        Other ->
            {Other, State}
    end;
run_decision(listTickets, _, _, State) ->
    {scrub:call(service(decision, State), "listTickets", ["owner"]), State};
run_decision(getTickets, _, _, State) ->
    {scrub:call(service(decision, State), "getTickets", ["creator"]), State};
run_decision(getAllTickets, _, _, State) ->
    {scrub:call(service(decision, State), "getAllTickets", []), State};
run_decision(deleteTicket, _, _, #state{tickets=[Id|Rest]}=State) ->
    case scrub:call(service(decision, State), "deleteTicket", [Id]) of
        {ok, _, _}=Reply -> {Reply, State#state{tickets=Rest}};
        Other            -> {Other, State}
    end;
run_decision(takeOwnership, _, _, #state{tickets=[Id|_]}=State) ->
    {scrub:call(service(decision, State), "takeOwnership", [Id, "owner"]),
     State};
run_decision(requestEval, _, _, #state{tickets=[Id|_]}=State) ->
    {scrub:call(service(decision, State), "requestEval",
               [Id, random_string(255)]),
     State};
run_decision(getTicketHistory, _, _, #state{tickets=[Id|_]}=State) ->
    {scrub:call(service(decision, State), "getTicketHistory", [Id]),
     State};
run_decision(getTicket, _, _, #state{tickets=[Id|_]}=State) ->
    {scrub:call(service(decision, State), "getTicket", [Id]),
     State};
run_decision(getAttachment, _, _,
             #state{attachments=[{Ticket,Att}|_]}=State) ->
    {scrub:call(service(decision, State), "getAttachment", [Ticket, Att]),
     State};
run_decision(evalEta, _, _, #state{tickets=[Id|_]}=State) ->
    {scrub:call(service(decision, State), "evalEta",
               [Id, iso_8601_fmt(calendar:now_to_local_time(now()))]),
     State};
run_decision(deleteDoc, _, _,
             #state{attachments=[{Ticket,Att}|Rest]}=State) ->
    case scrub:call(service(decision, State), "deleteDoc", [Ticket, Att]) of
        {ok, _, _}=Reply -> {Reply, State#state{attachments=Rest}};
        Other            -> {Other, State}
    end;
run_decision(attachDoc, _, ValueGen,
             #state{tickets=[Id|_],attachments=Atts}=State) ->
    Name = random_string(20),
    case scrub:call(service(decision, State), "attachDoc",
                [Id, Name, "image/bmp", base64:encode_to_string(ValueGen())]) of
        {ok, _, _}=Reply -> {Reply, State#state{attachments=[{Id, Name}|Atts]}};
        Other            -> {Other, State}
    end;
run_decision(addDecission, _, _, #state{tickets=[Id|_]}=State) ->
    Decision = random_string(6),
    {scrub:call(service(decision, State), "addDecission",
               [Id, Decision, "decidor"]),
     State};
run_decision(_Op, _, _, State) ->
    {silent, State}.

run_admin(isGroupAllowed, _, _, State) ->
    Response = detergent:call(service(admin,State), "isGroupAllowed",
                              [random_string(16), random_string(20)]),
    {Response, State};
run_admin(Op, _, _, State) ->
    Response = detergent:call(service(admin,State), atom_to_list(Op), []),
    {Response, State}.
%% ====================================================================
%% Internal functions
%% ====================================================================
random_string(Length) ->
    [random:uniform(93)+33 || _ <- lists:seq(1,Length)].

username(Id) -> "benchmarkuser_" ++ integer_to_list(Id).
password(Id) -> "password" ++ integer_to_list(Id).

service(users, State) -> State#state.users;
service(roles, State) -> State#state.role;
service(params, State) -> State#state.param;
service(decision, State) -> State#state.decision;
service(damages, State) -> State#state.damage;
service(admin, State) -> State#state.admin.

prepare_wsdls() ->
    UsersEndpoint = basho_bench_config:get(users_endpoint),
    UsersWsdl = detergent:initModel(UsersEndpoint),
    RolesEndpoint = basho_bench_config:get(roles_endpoint),
    RolesWsdl = detergent:initModel(RolesEndpoint),
    ParamsEndpoint = basho_bench_config:get(params_endpoint),
    ParamsWsdl = detergent:initModel(ParamsEndpoint),
    DamageEndpoint = basho_bench_config:get(damage_endpoint),
    DamageWsdl = detergent:initModel(DamageEndpoint),
    DecisionEndpoint = basho_bench_config:get(decision_endpoint),
    DecisionWsdl = scrub:initModel(DecisionEndpoint),
    AdminEndpoint = basho_bench_config:get(admin_endpoint),
    AdminWsdl = detergent:initModel(AdminEndpoint),
    #state{users = UsersWsdl,
           role = RolesWsdl,
           param = ParamsWsdl,
           damage = DamageWsdl,
           decision=DecisionWsdl,
           admin = AdminWsdl}.

iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
                                [Year, Month, Day, Hour, Min, Sec])).
