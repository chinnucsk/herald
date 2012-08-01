RUN := -name herald@`hostname` -pa `pwd`/ebin `pwd`/deps/*/ebin

REBAR = ./rebar

all:
	${REBAR} get-deps && ${REBAR} compile

clean:
	${REBAR} clean

build_plt: all
	dialyzer -pa deps/*/ebin --apps exmpp erts kernel stdlib inets --output_plt ~/.herald_dialyzer_plt --build_plt

analyze: all
	dialyzer -pa deps/*/ebin --plt ~/.herald_dialyzer_plt -Wunmatched_returns -Werror_handling -Wbehaviours ebin

update-deps:
	rebar update-deps

run: all
	erl -config app.config -boot start_sasl ${RUN}
