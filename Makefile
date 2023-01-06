all: gnew gpull gpackage gsystem zjabbar

zjabbar:
	guix home reconfigure /home/zjabbar/code/guix-channel/zaijab/home/zjabbar.scm --allow-downgrades -v 4

gsystem:
	sudo guix system reconfigure /home/zjabbar/code/guix-channel/zaijab/systems/$(HOSTNAME).scm --allow-downgrades

gpull:
	guix pull --channels=/home/zjabbar/code/guix-channel/zaijab/channels.tmpl --allow-downgrades -v 4
	guix upgrade

gpackage:
	guix package -m /home/zjabbar/code/guix-channel/zaijab/minimal-manifest.tmpl

gnew:
	git add -A
	git diff-index --quiet HEAD || git commit -am "Updating Config"
	git push -u github main
