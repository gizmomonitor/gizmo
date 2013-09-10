.PHONY:

all:
	make clean
	make compile
	make update_riak_code

compile:
	./rebar get-deps compile

test:
	make compile
	ct_run -pa apps/*/ebin -pa deps/*/ebin -dir apps/*/test/ -logdir tests -cover cover.spec

release:
	make all
	./rebar generate overlay_vars=${NODE}.config

clean:
	./rebar clean
	rm -rf apps/*/deps
	rm -rf deps/*/deps
	rm -rf apps/*/doc
	rm -rf apps/*/logs
	rm -rf deps/*/logs
	find . -name "*~" -exec rm {} \;
	find . -name ".#*" -exec rm {} \;
	find . -name "erl_crash.dump" -exec rm {} \;
	find . -name "#*#" -exec rm {} \;

doc:
	rebar doc skip_deps=true

update_riak_code:
	riak-admin erl_reload

dialyzer:
	dialyzer --output_plt .deps_plt --build_plt --apps erts kernel stdlib
	dialyzer --fullpath --plt .deps_plt -Wrace_conditions -r ./apps/*/ebin