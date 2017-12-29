%%%-----------------------------------------------
%%% @author chenyansheng
%%% @date 2017-12-20
%%% @doc 宏定义头文件
%%%-----------------------------------------------
-ifndef(DEFINES_HRL).
-define(DEFINES_HRL, true).

%% 类型转换
-define(B2S(B), erlang:binary_to_list(B)).
-define(S2B(S), erlang:list_to_binary(S)).
-define(N2S(N), erlang:integer_to_list(N)).
-define(S2N(S), erlang:list_to_integer(S)).
-define(A2S(A), erlang:atom_to_list(A)).
-define(U2B(Text), (Text)).

%% 打印日志
-define(ERROR(Format), logger_handler:error_msg(?MODULE, ?LINE, Format, [])).
-define(ERROR(Format, Data), logger_handler:error_msg(?MODULE, ?LINE, Format, Data)).
-define(INFO(Format), logger_handler:info_msg(?MODULE, ?LINE, Format, [])).
-define(INFO(Format, Data), logger_handler:info_msg(?MODULE, ?LINE, Format, Data)).

%% 打印
-define(P(D),  io:format(?U2B("[INFO] L~p: "++D++"\n"), [?LINE])).
-define(P(F, D), io:format(?U2B("[INFO] L~p: "++F++"\n"), [?LINE|D])).

-endif.
