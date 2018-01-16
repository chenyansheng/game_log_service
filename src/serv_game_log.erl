%%%-----------------------------------------------
%%% @author chenyansheng
%%% @date 2017-12-20
%%% @doc game log Service
%%%-----------------------------------------------
-module(serv_game_log).
-behaviour(gen_server).

-include("defines.hrl").

-export([start_link/10, get_state/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% state
-record(state, {server_name, 
                mysql_pool_id, 
                agent_id, 
                server_id, 
                ip, 
                db_port, 
                db_user, 
                db_pwd, 
                db_name, 
                sync_tab}).

%% 5min定时器
-define(WORKER_TIMER, 300000).

%%-------------------
%% public fun
%%-------------------
%% @doc 启动
start_link(ServerName, DbPoolId, AgentId, ServerId, Ip, DbPort, DbUser, DbPwd, DbName, SyncTab) ->
    gen_server:start_link({local, ServerName}, ?MODULE, 
        [ServerName, DbPoolId, AgentId, ServerId, Ip, DbPort, DbUser, DbPwd, DbName, SyncTab], []).

%% @doc 获取状态
get_state(ServerRef) ->
    call(ServerRef, get_state).


%%--------------------
%% callback fun
%%--------------------
init([ServerName, DbPoolId, AgentId, ServerId, Ip, DbPort, DbUser, DbPwd, DbName, SyncTab] = _Args) ->
    erlang:process_flag(trap_exit, true),
    State = #state{server_name = ServerName,
                   mysql_pool_id = DbPoolId,
                   agent_id = AgentId,
                   server_id = ServerId,
                   ip = Ip,
                   db_port = DbPort,
                   db_user = DbUser,
                   db_pwd = DbPwd,
                   db_name = DbName,
                   sync_tab = SyncTab},
    start_worker_timer(State),
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
    % ?P("do_loop: ~p", [State]),
    start_worker_timer(State);

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
start_worker_timer(State) ->
    dumper_worker(State),
    erlang:send_after(?WORKER_TIMER, self(), 'do_loop'),
    ok.

%% 查询数据
dumper_worker(#state{mysql_pool_id = DbPoolId, sync_tab = SyncTab}) ->
    StartTime = util:now_sec() - ?WORKER_TIMER / 1000,
    EndTime = util:now_sec(),
    Where = "m_time >=" ++ ?N2S(StartTime) ++ " AND m_time <" ++ ?N2S(EndTime),
    [begin
        {selected, _, Data} = (db_function:new(DbPoolId)):select(OneTab, "*", Where),
        [write_log(OneTab, Elem) || Elem <- Data]
    end || OneTab <- SyncTab].

% 写文件
% Data 是一维列表
write_log(Prefix, Data) ->
    {ok, Fd} = open_log_file(Prefix),
    Str = do_join(Data, ","),
    Data2 = [Str, "\n"],
    case file:write(Fd, Data2) of
        ok ->
            ok;
        {error, Reason} ->
            throw({error, Reason})
    end.

%% 添加分隔符
do_join([], Sep) when is_list(Sep) ->
    []; 
do_join([H|T], Sep) ->
    [util:any_to_iodata(H)] ++ [[Sep, util:any_to_iodata(X)] || X <- T].

%% 打开日志文件
open_log_file(Prefix) ->
    LogFile = get_filename(Prefix),
    ok = filelib:ensure_dir(LogFile),
    case file:open(LogFile, [raw, append, {delayed_write, 65536, 3000}]) of
        {ok, FdNew} ->
            {ok, FdNew};
        {error, Reason} ->
            throw({error, Reason})
    end.

%% 获取文件名
get_filename(Prefix) ->
    {{Y, Mon, D}, {H, M, _S}} = erlang:localtime(),
    LogDir = util:get_game_log_dir(),
    Suffix = ".csv",
    FileName = lists:concat([Prefix, "_", Y, two_digits(Mon), two_digits(D), two_digits(H), two_digits(M), Suffix]),
    filename:join([LogDir, FileName]).

%% 两位
two_digits(N) when N < 10 ->
    [$0, $0 + N];
two_digits(N) when N < 100 ->
    erlang:integer_to_list(N).
