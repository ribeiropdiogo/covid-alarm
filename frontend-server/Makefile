recompile:
	./rebar3 escriptize

compile:
	wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
	./rebar3 escriptize

run:
	_build/default/bin/frontend_server

clean:
	rm -f rebar3
	rm -rf _build
