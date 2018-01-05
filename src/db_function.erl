%%%-----------------------------------------------
%%% @author chenyansheng
%%% @date 2018-01-04
%%% @doc mysql db function
%%%-----------------------------------------------
-module(db_function, [POOL]).
-include("pmod.hrl").

-include("defines.hrl").

-export([select/2, select/3, select/4, select/5]).


%% @doc 查询请求
select(Table, Fields) ->
    select(Table, Fields, "").

%% @doc 查询请求(字段没有添加``)
%% select * from tab where id = 1;
%% select("tab", "*", "id=1")
select(Table, Fields, Where) ->
    select(Table, Fields, Where, "").

%% 查询请求
select(Table, Fields, Where, GroupBy) ->
    select(Table, Fields, Where, GroupBy, "").

select(Table, Fields, Where, GroupBy, Limit) 
        when is_list(Table), is_list(Fields), is_list(Where), is_list(GroupBy), is_list(Limit) ->
    Sql = [<<"SELECT ">>,
    Fields,
    <<" FROM ">>, Table, 
    begin
        if 
            length(Where) > 0 ->
                [<<" WHERE ">>, Where];
            true ->
                <<>>
        end
    end,
    " ",
    begin
        if
            length(GroupBy) > 0 ->
                [<<" GROUP BY ">>, GroupBy];
            true ->
                <<>>
        end
    end,
    " ",
    begin
        if
            length(Limit) > 0 ->
                [<<" LIMIT ">>, Limit];
            true ->
                <<>>
        end
    end
    ],
    ?P("sql is: ~s", [iolist_to_binary(Sql)]),
    fetch(Sql).




%%----------------------
%% private fun
%%----------------------
%% @doc 执行一条sql语句
%% 返回{selected, Fields, Rows} 
%% | {error, Reason}
%% | {updated, AffectedRows, InsertId}
fetch(Query) ->
    fetch(Query, ?DB_EXECUTE_TIMEOUT).

fetch(Query, Timeout) ->
    handle_result(catch mysql:fetch(POOL, Query, Timeout)).

%% 对结果进行处理
handle_result({data, Result}) ->
    {selected, mysql:get_result_field_info(Result), mysql:get_result_rows(Result)};
handle_result({updated, Result}) ->
    {updated, mysql:get_result_affected_rows(Result)};
handle_result({error, Result}) when is_tuple(Result) ->
    Reason = mysql:get_result_reason(Result),
    ?ERROR("mysql error:~s", [Reason]),
    {error, Reason};
handle_result({'EXIT', {Reason, _}}) ->
    ?ERROR("mysql error:~w", [Reason]),
    {error, Reason}.
