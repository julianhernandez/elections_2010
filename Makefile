ERL ?= erl
APP := elections_2010

all:
	@rebar compile

clean:
	@rebar clean

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
