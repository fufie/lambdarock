all: 

clean:
	cd libs && $(MAKE) clean
	cd langband && $(MAKE) clean
	$(RM) *.o lbui.so dircall.so lbui.dll dircall.dll *.fasl *.a *~ lbsd
