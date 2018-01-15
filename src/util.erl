%%%-----------------------------------------------
%%% @author chenyansheng
%%% @date 2017-12-20
%%% @doc 辅助工具函数
%%%-----------------------------------------------
-module(util).

-include("defines.hrl").

-export([get_current_dir/0, get_config_file/0, get_game_log_dir/0]).
-export([any_to_iodata/1]).
-export([start_child/2]).

%% @doc 获取当前目录
get_current_dir() ->
    {ok, Dir} = file:get_cwd(),
    Dir.

%% @doc 获取配置文件
get_config_file() ->
    RootDir = get_current_dir(),
    ConfDir = filename:join([RootDir, "config"]),
    ConfFile = ConfDir ++ "/config.conf",
    ConfFile.

%% @doc 获取game_log目录
get_game_log_dir() ->
    RootDir = get_current_dir(),
    LogDIr = filename:join([RootDir, "game_log"]),
    LogDIr.

%% @doc 转化成iodata
any_to_iodata(Any) when is_list(Any) ->
    Any;
any_to_iodata(Any) when is_binary(Any) ->
    Any;
any_to_iodata(Any) when is_atom(Any) ->
    erlang:atom_to_list(Any);
any_to_iodata(Any) when is_integer(Any) ->
    erlang:integer_to_list(Any);
any_to_iodata(Any) when is_float(Any) ->
    erlang:float_to_list(Any).

%% @doc 启动某个子服务
start_child(Sup, Child) ->
    case catch supervisor:start_child(Sup, Child) of
        {ok, _} ->
            ok;
        {error, {{already_started, _Pid}, _}} ->
            ok;
        Other ->
            io:format("supervisor start child faild: ~p", [Other]),
            throw(Other)
    end.
