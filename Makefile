
all:
	(cd src; $(MAKE))
	(./auto_test.sh src/lus2rs)
	# (cd examples; $(MAKE))

compile:
	(cd src; $(MAKE))

test:
	(./auto_test.sh src/lus2rs)

clean:
	(cd src; $(MAKE) clean)
