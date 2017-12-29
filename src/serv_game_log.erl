%%%-----------------------------------------------
%%% @author chenyansheng
%%% @date 2017-12-20
%%% @doc game log Service
%%%-----------------------------------------------
-module(serv_game_log).
-behaviour(gen_server).

-include("defines.hrl").

-export([start_link/8, get_state/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% state
-record(state, {server_name, agent_id, server_id, ip, db_port, db_user, db_pwd, db_name}).

%% 5min定时器
-define(WORKER_TIMER, 3000).

%%-------------------
%% public fun
%%-------------------
%% @doc 启动
start_link(ServerName, AgentId, ServerId, Ip, DbPort, DbUser, DbPwd, DbName) ->
    gen_server:start_link({local, ServerName}, ?MODULE, 
        [ServerName, AgentId, ServerId, Ip, DbPort, DbUser, DbPwd, DbName], []).

%% @doc 获取状态
get_state(ServerRef) ->
    call(ServerRef, get_state).

%%--------------------
%% callback fun
%%--------------------
init([ServerName, AgentId, ServerId, Ip, DbPort, DbUser, DbPwd, DbName] = _Args) ->
    erlang:process_flag(trap_exit, true),
    State = #state{server_name = ServerName,
                   agent_id = AgentId,
                   server_id = ServerId,
                   ip = Ip,
                   db_port = DbPort,
                   db_user = DbUser,
                   db_pwd = DbPwd,
                   db_name = DbName},
    start_worker_timer(),
    {ok, State}.

handle_call(get_state, _From, State) ->
    {reply, State};
handle_call(_Msg, _From, State) ->
    ?ERROR("unknown call: ~p", [_Msg]),
    {noreply, State}.

handle_cast(_Msg, State) ->
    ?ERROR("unknown cast: ~p", [_Msg]),
    {noreply, State}.

handle_info('do_loop', State) ->
    ?P("do_loop: ~p", [State]),
    start_worker_timer();

handle_info(_Info, State) ->
    ?ERROR("unknown info: ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

%%----------------------
%% private fun
%%----------------------
%% call调用
call(ServerRef, Req) ->
    gen_server:call(ServerRef, Req, 5000).

%% 启动worker定时器
start_worker_timer() ->
    erlang:send_after(?WORKER_TIMER, self(), 'do_loop'),
    ok.
