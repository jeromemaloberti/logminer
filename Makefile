SUFFIXES = .rl .dot
RAGEL = ragel

all: xensource.ml _obuild/logminer/logminer.asm xensource.dot

_obuild/logminer/logminer.asm: xensource.ml logminer.ocp *.ml
	ocp-build
xensource.dot: xensource.rl
	$(RAGEL) $(RAGELFLAGS) -V -p $< -o $@

xensource.ml: xensource.rl date_time.rl
	$(RAGEL) $(RAGELFLAGS) -O -G0 $< -o $@

session_parser.dot: session_parser.rl
	$(RAGEL) $(RAGELFLAGS) -V -p $< -o $@

session_parser.ml: session_parser.rl
	$(RAGEL) $(RAGELFLAGS) -O -G0 $< -o $@

