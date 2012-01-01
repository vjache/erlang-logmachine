
export ERL_LIBS=../

erl -args_file "vm.args" -name logmach -config dev.config -eval "application:start(logmachine)"