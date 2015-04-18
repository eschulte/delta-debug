# Use buildapp as the lisp compiler
LC:=buildapp

# You can set this as an environment variable to point to an alternate
# quicklisp install location.  If you do, ensure that it ends in a "/"
# character, and that you use the $HOME variable instead of ~.
QUICK_LISP?=$(HOME)/quicklisp/
ifeq "$(wildcard $(QUICK_LISP)/setup.lisp)" ""
$(warning $(QUICK_LISP) does not appear to be a valid quicklisp install)
$(error Please point QUICK_LISP to your quicklisp installation)
endif

LISP_LIBS+= delta-debug-exe
LC_LIBS:=$(addprefix --load-system , $(LISP_LIBS))

# Flags to buildapp
QUIT=(lambda (error hook-value)
QUIT+=(declare (ignorable hook-value))
QUIT+=(format *error-output* \"ERROR: ~a~%\" error)
QUIT+=\#+sbcl (sb-ext:exit :code 2) \#+ccl (quit 2))
LCFLAGS=--manifest-file $(QUICK_LISP)/local-projects/system-index.txt \
	--asdf-tree $(QUICK_LISP)/dists/quicklisp/software \
	--eval "(setf *debugger-hook* $(QUIT))" \
	$(LC_LIBS)

ifneq ($(LISP_STACK),)
LCFLAGS+= --dynamic-space-size $(LISP_STACK)
endif

# Compiled lisp executables
LISP_EXES=delta
LISP_BINS=$(addprefix bin/, $(LISP_EXES))

all: delta
.PHONY:  clean check check-flat check-patch

$(CL_SETUP):
	echo "(load \"$(QUICK_LISP)/setup.lisp\")" >$@

delta: delta-debug-exe.lisp
	$(LC) $(LCFLAGS) --output $@ --entry "delta-debug-exe:main"

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
