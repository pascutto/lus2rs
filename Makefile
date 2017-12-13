
all:
	(cd src; $(MAKE))
	(./auto_test.sh src/lus2rs)
	# (cd examples; $(MAKE))

compile:
	(cd src; $(MAKE))

test:
	(cd examples; $(MAKE))

clean:
	(cd src; $(MAKE) clean)
	(cd examples; $(MAKE) clean)
