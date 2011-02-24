all:
	rebar get-deps && rebar compile
	erl -pa ebin -noinput +B -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

clean:
	rebar clean

build_plt: all
	rebar skip_deps=true build-plt

analyze: all
	rebar dialyze

update-deps:
	rebar update-deps

doc: all
	rebar skip_deps=true doc

xref: all
	rebar skip_deps=true xref
	
run: all
	if [ -f `hostname`.config ]; then\
		erl  +Bc +K true +W w -smp enable -config `hostname` -name elog -boot start_sasl -pa deps/riak_err/ebin -pa ebin -s crypto -s inets -s ssl -s elog;\
	else\
		erl  +Bc +K true +W w -smp enable -name elog -boot start_sasl -pa deps/riak_err/ebin -pa ebin -s crypto -s inets -s ssl -s elog;\
	fi

shell: all
	if [ -f `hostname`.config ]; then\
		erl  +Bc +K true +W w -smp enable -config `hostname` -name elog -boot start_sasl -pa deps/riak_err/ebin -pa ebin -s crypto -s inets -s ssl;\
	else\
		erl  +Bc +K true +W w -smp enable -name elog -boot start_sasl -pa deps/riak_err/ebin -pa ebin -s crypto -s inets -s ssl;\
	fi

test: all
	if [ -f `hostname`.config ]; then\
		erl -noshell -noinput +Bc +K true -smp enable -config `hostname` -name elog -pa ebin -s crypto -s inets -s ssl -s elog -run elog_tester main;\
	else\
		erl -noshell -noinput +Bc +K true -smp enable -name elog -pa ebin -s crypto -s inets -s ssl -s elog -run elog_tester main;\
	fi
