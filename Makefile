LISP:="sbcl ccl"
CLC:=cl-launch
QUICK_LISP?=$(HOME)/quicklisp/
CL_SETUP=cl-launch-setup.lisp
CLFLAGS=--no-include --system delta-debug-exe --lisp $(LISP) --dump '!' -f $(CL_SETUP)

all: delta
.PHONY:  clean check check-flat check-patch

$(CL_SETUP):
	echo "(load \"$(QUICK_LISP)/setup.lisp\")" >$@

delta: delta-debug-exe.lisp $(CL_SETUP)
	$(CLC) $(CLFLAGS) --output $@ -r delta-debug-exe:main

flat-script.sh:
	@echo "#!/bin/sh" >$@
	@echo 'sh $$@ >/dev/null' >>$@
	@echo "if [ \$$? -eq 89 ];then exit 0;else exit 1;fi" >>$@
	@chmod +x $@

test-file.sh:
	@echo "#!/bin/sh" >$@
	@seq 100|sed 's/^/echo /;89s/echo/exit/' >>$@

check-flat: delta flat-script.sh test-file.sh
	@echo -en "$@:\t"
	@if [ "$$(./$^)" == "exit 89" ];then echo "PASS"; else echo "FAIL";fi

patch-script.sh:
	@echo "#!/bin/sh" >$@
	@echo 'sh $$@ >/dev/null' >>$@
	@chmod +x $@

test-file-new.sh: test-file.sh
	@sed 's/exit 89/# removed bad exit/;5,10d' $< > $@
	@echo "# trash at the end" >> $@

test-file.patch: test-file.sh test-file-new.sh
	@diff -u $^ > $@ || exit 0

check-patch: delta patch-script.sh test-file.patch
	@echo -en "$@:\t"
	@MIN="$$(./$^)"; \
	if $$(echo "$$MIN"|grep -q "removed bad exit") && \
	   $$(echo "$$MIN"|grep -vq "trash") && \
	   $$(echo "$$MIN"|grep -vq "echo 5");then echo "PASS"; else echo "FAIL";fi

check: check-flat check-patch

clean:
	@rm -f $(CL_SETUP) *.fasl *.lx32fsl
	@rm -f flat-script.sh test-file.sh test-file-new.sh
	@rm -f patch-script.sh patch-script-new.sh test-file.patch

real-clean: clean
	@rm -f delta
