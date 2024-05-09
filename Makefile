install:
	go build -o /usr/local/bin/daicker src/main.go
uninstall:
	rm /usr/local/bin/daicker
build-parser:
	goyacc -o src/lang/parser.go src/lang/parser.go.y
