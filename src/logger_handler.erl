%%%-----------------------------------------------
%%% @author chenyansheng
%%% @date 2017-12-20
%%% @doc error logger
%%%-----------------------------------------------
-module(logger_handler).

-include("defines.hrl").

-export([error_msg/4, info_msg/4]).

%% @doc Sends a standard error report event to the error logger
error_msg(Module, Line, Format, Data) ->
    Format2 = "Module:" ++ ?A2S(Module) ++ " | Line:" ++ ?N2S(Line) ++ " : \n" ++ Format,
    Msg = io_lib:format(Format2, Data),
    error_logger:error_report(Msg),
    ok.

%% @doc Sends a standard information event to the error logger
info_msg(Module, Line, Format, Data) ->
    Format2 = "Module:" ++ ?A2S(Module) ++ " | Line:" ++ ?N2S(Line) ++ " : \n" ++ Format,
    Msg = io_lib:format(Format2, Data),
    error_logger:info_report(Msg),
    ok.
