%% -*- mode: erlang; -*-
{application, logmachine,
 [{description, "An Erlang Log Machine"},
  {vsn, "0.0.1"},
  {modules, []},
  {registered, []},
  {build_dependencies, []},
  {env, []},
  {applications, [kernel, stdlib, sasl]},
  {mod, {logmachine_app, []}}]}.
