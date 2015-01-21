REMOTEUSER ?= mc301
REMOTEHOST ?= nighthawk.stat.duke.edu
REMOTEDIR ?= ~/.public_html/shinyed/
REMOTE ?= $(REMOTEUSER)@$(REMOTEHOST):$(REMOTEDIR)

.PHONY: clean
clean:
	rm -rf _site/*

push: build
	rsync -az _site/* $(REMOTE)

build:
	jekyll build

serve:
	jekyll serve --watch --config _config.yml,_local.yml