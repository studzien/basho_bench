-module(basho_bench_driver_xmpp).

%% basho_bench_driver callbacks
-export([new/1, run/4]).

%% random message text generator
-export([text/2]).

%% slave message receiver process
-export([init_receiver/2]).

-include_lib("exml/include/exml.hrl").

-record(state, {user_prefix, password_prefix, server, conn, receiver}).

-define(TTD_TIMEOUT, 5000).

new(Id) ->
    init(),
    Server = basho_bench_config:get(xmpp_server, <<"localhost">>),
    UsernamePrefix = basho_bench_config:get(xmpp_username_prefix, <<"user">>),
    PasswordPrefix = basho_bench_config:get(xmpp_password_prefix, <<"password">>),
    IdB = list_to_binary(integer_to_list(Id)),
    Username = <<UsernamePrefix/binary, IdB/binary>>,
    Password = <<PasswordPrefix/binary, IdB/binary>>,
    Props = [{server, Server},
             {username, Username},
             {password, Password},
             {resource, <<"basho_bench">>}],

    Receiver = spawn_link(?MODULE, init_receiver, [self(), Props]),
    Response = receive 
        R -> R 
    after 5000 -> 
        {error, timeout}
    end,

    case Response of
        {ok, Conn} ->
            Presence = escalus_stanza:presence(<<"available">>),
            escalus_connection:send(Conn, Presence),
            basho_bench_driver_xmpp_users:insert(Id),
            State = #state{conn=Conn,
                           user_prefix=UsernamePrefix,
                           password_prefix=PasswordPrefix,
                           receiver=Receiver,
                           server=Server},
            {ok, State};
        {error, Reason} ->
            {error, Reason}
    end.

run(send, KeyGen, Value, #state{conn=Conn}=State) ->
    Stanza = message(KeyGen(), Value, State),
    escalus_connection:send(Conn, Stanza),
    {ok, State};
run(send_ttd, KeyGen, Value, #state{conn=Conn}=State) ->
    Key = basho_bench_driver_xmpp_users:get(KeyGen()),
    #xmlel{attrs=Attrs1} = Stanza1 = message(Key, Value, State),
    Attrs2 = [{<<"basho_bench">>,<<"ttd">>} | Attrs1],
    escalus_connection:send(Conn, Stanza1#xmlel{attrs=Attrs2}),
    receive
        ttd_response -> {ok, State}
    after ?TTD_TIMEOUT ->
        {error, timeout, State}
    end;
run(_Operation, _KeyGen, _Value, State) ->
    {ok, State}.

text(_Id, Length) ->
    List = [random:uniform(95)+31 || _ <- lists:seq(1,Length)],
    list_to_binary(List).

message(Key, Value, #state{user_prefix=Prefix, server=Server}) ->
    KeyB = list_to_binary(integer_to_list(Key)),
    Recipient = <<Prefix/binary, KeyB/binary, "@", Server/binary>>,
    escalus_stanza:chat_to(Recipient, Value).

init() ->
    application:start(escalus),
    case whereis(basho_bench_driver_xmpp_users) of
        undefined ->
            basho_bench_driver_xmpp_users:start_link();
        _ ->
            ok
    end.

%%
%% XMPP receiver process logic
%%
init_receiver(Master, Props) ->
    case escalus_connection:start(Props) of
        {ok, Conn, _Props} ->
            Master ! {ok, Conn},
            receive_stanza(Master, Conn);
        {error, Reason} ->
            Master ! {error, Reason}
    end.

receive_stanza(Master, Conn) ->
    receive
        {stanza, Conn, Stanza} ->
            handle_stanza(Master, Conn, Stanza),
            receive_stanza(Master, Conn);
        _ ->
            receive_stanza(Master, Conn)
    end.

handle_stanza(Master, Conn, Stanza) ->
    handle_stanza1(Master, Conn, Stanza,
                   has_attribute(Stanza, <<"ttd">>)).

handle_stanza1(_Master, Conn, Stanza, true) ->
    From = exml_query:attr(Stanza, <<"from">>),
    Stanza1 = escalus_stanza:chat_to(From, <<>>), 
    #xmlel{attrs=Attrs1} = Stanza1,
    Attrs2 = [{<<"basho_bench">>, <<"ttd_response">>} | Attrs1],
    escalus_connection:send(Conn, Stanza1#xmlel{attrs=Attrs2});
handle_stanza1(Master, Conn, Stanza, false) ->
    handle_stanza2(Master, Conn, Stanza,
                   has_attribute(Stanza, <<"ttd_response">>)).

handle_stanza2(Master, _Conn, _Stanza, true) ->
    Master ! ttd_response;
handle_stanza2(_Master, _Conn, _Stanza, false) ->
    ok.

has_attribute(Stanza, Attribute) ->
    exml_query:attr(Stanza, <<"basho_bench">>) =:= Attribute.
