%%%-----------------------------------------------
%%% @author chenyansheng
%%% @date 2017-12-20
%%% @doc supervisor
%%%-----------------------------------------------
-module(service_sup).
-behaviour(supervisor).

-include("defines.hrl").

-export([start_link/1, init/1]).

%% @doc start link
start_link(_Args) ->
    {ok, Sup} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    SyncServer = get_sync_server(),
    [begin
        [{server_name, ServerName},
         {agent_id, AgentId},
         {server_id, ServerId},
         {ip, Ip},
         {db_port, DbPort},
         {db_user, DbUser},
         {db_pwd, DbPwd},
         {db_name, DbName}] = OneServer,
         
        Child = {ServerName, {serv_game_log, start_link, 
            [ServerName, AgentId, ServerId, Ip, DbPort, DbUser, DbPwd, DbName]},
            permanent, 2000, worker,[serv_game_log]},
        ok = util:start_child(Sup, Child)
    end || OneServer <- SyncServer],
    {ok, Sup}.

%% @doc init
init([]) ->
    {ok, {{one_for_one, 10, 10}, []}}.

%% @doc 解析配置文件
get_sync_server() ->
    ConfFile = util:get_config_file(),
    {ok, Conf} = file:consult(ConfFile),
    SyncServer = proplists:get_value("SyncServer", Conf, []),
    SyncServer.
