LISP:="sbcl ccl"
CLC:=cl-launch
QUICK_LISP?=$(HOME)/quicklisp/
CL_SETUP=cl-launch-setup.lisp
CLFLAGS=--no-include --system delta-debug --lisp $(LISP) --dump '!' -f $(CL_SETUP)

all: delta

$(CL_SETUP):
	echo "(load \"$(QUICK_LISP)/setup.lisp\")" >$@

%: %.lisp $(CL_SETUP)
	$(CLC) $(CLFLAGS) --output $@ -r delta-debug:$@

clean:
	rm -f delta $(CL_SETUP) *.fasl *.lx32fsl
