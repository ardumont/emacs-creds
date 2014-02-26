VERSION=$$(grep "^;; Version: " creds.el | cut -f3 -d' ')
PACKAGE_FOLDER=creds-$(VERSION)
ARCHIVE=$(PACKAGE_FOLDER).tar
USER=ardumont

init:
	cask init

deps:
	cask

test:
	cask exec emacs -Q -batch \
			-l ert \
			-l ./launch-tests.el \
			-f ert-run-tests-batch-and-exit

clean:
	rm -rf *.tar $(PACKAGE_FOLDER)

pkg-el:
	cask package

prepare:
	mkdir -p $(PACKAGE_FOLDER)
	cp -r creds.el creds-pkg.el $(PACKAGE_FOLDER)

package: clean pkg-el prepare
	tar cvf $(ARCHIVE) $(PACKAGE_FOLDER)
	rm -rf $(PACKAGE_FOLDER)

release:
	./release.sh $(VERSION) $(USER)

info:
	cask info
