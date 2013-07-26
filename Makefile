LISP:="sbcl ccl"
CLC:=cl-launch
QUICK_LISP?=$(HOME)/quicklisp/
CL_SETUP=cl-launch-setup.lisp
CLFLAGS=--no-include --system delta-debug-exe --lisp $(LISP) --dump '!' -f $(CL_SETUP)

all: delta
.PHONY: check clean

$(CL_SETUP):
	echo "(load \"$(QUICK_LISP)/setup.lisp\")" >$@

delta: delta-debug-exe.lisp $(CL_SETUP)
	$(CLC) $(CLFLAGS) --output $@ -r delta-debug-exe:main

test-script.sh:
	@echo "#!/bin/sh" >$@
	@echo 'sh $$@ >/dev/null' >>$@
	@echo "if [ \$$? -eq 89 ];then exit 0;else exit 1;fi" >>$@
	@chmod +x $@

test-file.sh:
	@echo "#!/bin/sh" >$@
	@seq 100|sed 's/^/echo /;89s/echo/exit/' >>$@

check: delta test-script.sh test-file.sh
	@if [ "$$(./$^)" == "exit 89" ];then echo "PASS"; else echo "FAIL";fi

clean:
	rm -f delta $(CL_SETUP) test-file.sh test-script.sh *.fasl *.lx32fsl
