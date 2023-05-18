all: gnew gpull gpackage zjabbar #gsystem

zjabbar:
	guix home reconfigure /home/zjabbar/code/guix-channel/zaijab/home/zjabbar.scm --allow-downgrades

gsystem:
	sudo guix system reconfigure /home/zjabbar/code/guix-channel/zaijab/systems/based-system.scm --allow-downgrades

gpull:
	guix pull --channels=/home/zjabbar/code/guix-channel/zaijab/channels.tmpl --allow-downgrades
	guix upgrade

gpackage:
	guix package -m /home/zjabbar/code/guix-channel/zaijab/minimal-manifest.tmpl

gnew:
	git add -A
	git diff-index --quiet HEAD || git commit -am "Updating Config"
	git push -u github main


gc:
	guix gc -d 1m
