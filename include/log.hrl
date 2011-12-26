-define(SELF_APP, fun()->case application:get_application() of {ok,_App} -> _App; undefined -> undefined end end()).

-define(LOG_INFO(Report), error_logger:info_report([Report, {application, ?SELF_APP}, {module, ?MODULE}, {line, ?LINE}])).
-define(LOG_WARNING(Report), error_logger:warning_report([Report, {application, ?SELF_APP}, {module, ?MODULE}, {line, ?LINE}])).
-define(LOG_ERROR(Report), error_logger:error_report([Report, {application, ?SELF_APP}, {module, ?MODULE}, {line, ?LINE}])).

%% Echo macro for debugging purposes. Just a convenient helper to print something
-define(ECHO(Message), io:format("(~p|~p|~p): ~p~n", [self(),?MODULE, ?LINE, Message])).
