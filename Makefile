all: gnew gpull gsystem

gc:
	guix gc -d 1m

gnew:
	git add -A
	git diff-index --quiet HEAD || git commit -am "Updating Config"
	git push -u github main

gpull:
	guix pull --channels=/home/zjabbar/code/guix-channel/zaijab/channels.tmpl --allow-downgrades
	guix upgrade

gpackage:
	guix package -m /home/zjabbar/code/guix-channel/zaijab/minimal-manifest.tmpl --no-grafts

gsystem:
	sudo guix system reconfigure /home/zjabbar/code/guix-channel/zaijab/systems/based-system.scm --allow-downgrades --no-grafts

gshepherd_log:
	sudo cat /var/log/messages

init:
	mkdir ~/.mail/zaijab2000

pinephone-build:
	guix system image /home/zjabbar/code/guix-channel/zaijab/systems/pinephone.scm --no-grafts --image-type=rock64-raw -v 4

pinephone-write:
	sudo dd if=`guix system image --image-type=rock64-raw /home/zjabbar/code/guix-channel/zaijab/systems/pinephone.scm --no-grafts` \
	of=/dev/sda bs=1M oflag=direct,sync status=progress
