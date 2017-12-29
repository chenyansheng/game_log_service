%%%-----------------------------------------------
%%% @author chenyansheng
%%% @date 2017-12-29
%%% @doc mysql db handler
%%%-----------------------------------------------
-module(db_mysql).
-behaviour(gen_server).

-include("defines.hrl").

-export([start_link/6]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% state
-record(state, {}).

%%-------------------
%% public fun
%%-------------------
%% @doc 启动
start_link(PoolId, Host, Port, User, Password, Database) ->
    gen_server:start_link({local, PoolId}, ?MODULE, 
        [PoolId, Host, Port, User, Password, Database], []).


%%--------------------
%% callback fun
%%--------------------
init([PoolId, Host, Port, User, Password, Database] = _Args) ->
    erlang:process_flag(trap_exit, true),
    case mysql:start_link(PoolId, Host, Port, User, Password, Database, fun log/4, utf8) of
        {ok, _Pid} ->
            mysql_connect(PoolId, Host, Port, User, Password, Database),
            ?INFO("db: ~p connect success", [Database]);
        {error, {already_started, _}} ->
            ?ERROR("db: ~p already_started", [Database]),
            mysql_connect(PoolId, Host, Port, User, Password, Database),
            ?INFO("db: ~p connect success", [Database]);
        {error, _Reason} ->
            ?ERROR("db: ~p connect fail, reason is:", [Database, _Reason])
    end,
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    ?ERROR("unknown call: ~p", [_Msg]),
    {noreply, State}.

handle_cast(_Msg, State) ->
    ?ERROR("unknown cast: ~p", [_Msg]),
    {noreply, State}.

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
%% 连接mysql
mysql_connect(PoolId, Host, Port, User, Password, Database) ->
    {ok, _ConnPid} = mysql:connect(PoolId, Host, Port, User, Password, Database, utf8, true).

%% 定义mysql使用的log fun
log(Module, Line, Level, FormatFun) ->
    case Level of
        error ->
            {Format, Arguments} = FormatFun(),
            ?ERROR("~w:~b: "++ Format ++ "~n", [Module, Line] ++ Arguments);
        _ -> 
            ok
    end.
