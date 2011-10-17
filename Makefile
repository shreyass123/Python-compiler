SCALAC=fsc
all: run
	@echo "compiled"

lex: lexer.scala
	$(SCALAC)  lexer.scala
	

parse: parser.scala
	$(SCALAC) parser.scala

run: parse 
	@pylex | scala PyParse

clean:
	rm -rf compiled pyparse sdiff
	@echo "insert code to clean here"

sdiff: sdiff.rkt
	raco exe sdiff.rkt

test: parse sdiff
	for i in tests/*.py; do make parse < $$i > $$i.out; ./sdiff $$i.out $$i.expected; done
