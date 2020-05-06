default: .tested

.tested: database/*.scm test/*.scm
	rm -rf ~/.cache/guile
	hdt
	touch $@

clean:
	rm -rf .tested

GUILE ?= guile

GUILE_SITE_DIR ?= $(shell $(GUILE) -c "(display (%site-dir)) (newline)")

install:
	install -D --target-directory $(GUILE_SITE_DIR)/database --mode 644 database/*

