install:
	stack --local-bin-path /usr/local/bin/ install
uninstall:
	rm /usr/local/bin/daicker
build-parser:
	goyacc -o src/lang/parser.go src/lang/parser.go.y
