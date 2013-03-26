SUFFIXES = .rl .dot
RAGEL = ragel
PREFIX?=/usr/local

.DEFAULT: all
all: dist/setup
	obuild build

dist/setup:
	obuild configure

clean:
	obuild clean

xensource.dot: lib/xensource.rl
	$(RAGEL) $(RAGELFLAGS) -V -p $< -o $@

lib/xensource.ml: lib/xensource.rl lib/date_time.rl
	$(RAGEL) $(RAGELFLAGS) -O -F1 $< -o $@

message.dot: lib/message.rl
	$(RAGEL) $(RAGELFLAGS) -V -p $< -o $@

lib/message.ml: lib/message.rl
	$(RAGEL) $(RAGELFLAGS) -O -F1 $< -o $@

session_parser.dot: lib/session_parser.rl
	$(RAGEL) $(RAGELFLAGS) -V -p $< -o $@

lib/session_parser.ml: lib/session_parser.rl
	$(RAGEL) $(RAGELFLAGS) -O -F1 $< -o $@

install: all
	ocamlfind install logminer META dist/build/lib-logminer/*.cma dist/build/lib-logminer/*.cmi dist/build/lib-logminer/*.cmxa dist/build/lib-logminer/*.a
	cp dist/build/logfilter/logfilter $(PREFIX)/bin/

uninstall:
	ocamlfind remove logminer
	rm $(PREFIX)/bin/logfilter

