CLEANFILES=*.o *.so *.ofasl *.fasl *.faslmt *.x86f *.err *.fas *.fsl *.ufsl *.lib *.sparcf *~ *.dfsl

all: 


lisp-clean:
	if test -d contraband; then cd contraband && $(MAKE) lisp-clean; fi
	if test -d vanilla; then cd vanilla && $(MAKE) lisp-clean; fi
	if test -d evomyth; then cd evomyth && $(MAKE) lisp-clean; fi

clean: lisp-clean
	$(RM) $(CLEANFILES)
