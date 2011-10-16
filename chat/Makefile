all:
	@echo Compilando
	@erlc *.erl

chat_server: all
	erl -s chat_server start

chat_client: all
	erl -s chat_client test

clean:
	rm -rf *.beam erl_crash.dump
