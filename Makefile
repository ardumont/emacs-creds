VERSION=$$(grep "^;; Version: " creds.el | cut -f3 -d' ')
PACKAGE_FOLDER=creds-$(VERSION)
ARCHIVE=$(PACKAGE_FOLDER).tar
USER=ardumont

pr:
	hub pull-request -b ardumont:master

clean-dist:
	rm -rf dist/

clean: clean-dist
	rm -rf *.tar

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

package: clean pkg-el
	cp dist/$(ARCHIVE) .
	make clean-dist

release:
	./release.sh $(VERSION) $(USER)

info:
	cask info
