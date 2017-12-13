
all:
	(cd src; $(MAKE))
	(cd examples; $(MAKE))

compile:
	(cd src; $(MAKE))

test:
	(cd examples; $(MAKE))

clean:
	(cd src; $(MAKE) clean)
	(cd examples; $(MAKE) clean)
