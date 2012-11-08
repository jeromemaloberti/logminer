SUFFIXES = .rl .dot
RAGEL = ragel

all: xensource.ml _obuild/logminer/logminer.asm xensource.dot

_obuild/logminer/logminer.asm: xensource.ml logminer.ocp *.ml
	ocp-build
xensource.dot: xensource.rl
	$(RAGEL) $(RAGELFLAGS) -V -p $< -o $@

xensource.ml: xensource.rl date_time.rl message.rl
	$(RAGEL) $(RAGELFLAGS) -O -G0 $< -o $@

session_parser.dot: session_parser.rl
	$(RAGEL) $(RAGELFLAGS) -V -p $< -o $@

session_parser.ml: session_parser.rl
	$(RAGEL) $(RAGELFLAGS) -O -G0 $< -o $@

install: all
	ocamlfind install logminer META _obuild/logminer/*.cma _obuild/logminer/*.cmi _obuild/logminer/*.cmxa _obuild/logminer/*.a

uninstall:
	ocamlfind remove logminer
