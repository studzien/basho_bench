-module(basho_bench_driver_xmpp_users).

-compile({no_auto_import, [get/1]}).

-behaviour(gen_server).

%% API
-export([start_link/0,
         get/1,
         insert/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {users}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(Id) ->
    gen_server:call(?MODULE, {get, Id}).

insert(User) ->
    gen_server:call(?MODULE, {insert, User}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Users = array:new(),
    {ok, #state{users=Users}}.

handle_call({insert, User}, _From, #state{users=Users}=State) ->
    Size = array:size(Users),
    NewUsers = array:set(Size, User, Users),
    {reply, ok, State#state{users=NewUsers}};
handle_call({get, Id}, _From, #state{users=Users}=State) ->
    Size = array:size(Users),
    Reply = array:get(Id rem Size, Users),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
