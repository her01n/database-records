default: .tested

.tested: database/*.scm test/*.scm
	rm -rf ~/.cache/guile
	hdt
	touch $@
