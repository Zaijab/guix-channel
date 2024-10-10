image=$(shell guix system image /home/zjabbar/code/guix-channel/zaijab/systems/pinephone.scm --no-grafts --image-type=rock64-raw)

all: git pull_from_lock system

all_update_lock: git update_lock system

gc:
	guix gc -d 1m

git:
	git add -A
	git diff-index --quiet HEAD || git commit -am "Updating Config"
	git push -u github main

pull:
	guix pull --channels=/home/zjabbar/code/guix-channel/zaijab/channel.scm --allow-downgrades -v 4

pull_from_lock:
	guix pull --channels=/home/zjabbar/code/guix-channel/zaijab/files/channel.tmpl --allow-downgrades

update_lock:
	guix pull --channels=/home/zjabbar/code/guix-channel/zaijab/channel.scm --allow-downgrades
	guix describe --format=channels > /home/zjabbar/code/guix-channel/zaijab/files/channel_lock.tmpl

system:
	sudo guix system reconfigure -e '(@ (zaijab systems based-system) my-operating-system)' --allow-downgrades

system_from_file:
	sudo guix system reconfigure /home/zjabbar/code/guix-channel/zaijab/systems/based-system.scm --allow-downgrades -v 4 --no-grafts

print_shepherd_log:
	sudo cat /var/log/messages

init:
	mkdir ~/.mail/zaijab2000

pinephone-build:
	guix system image /home/zjabbar/code/guix-channel/zaijab/systems/pinephone.scm --no-grafts --image-type=rock64-raw -v 4

pinephone-save:
	cp ${image} /tmp/my-image.qcow2
	chmod +w /tmp/my-image.qcow2

pinephone-write:
	sudo dd if=`guix system image --image-type=rock64-raw /home/zjabbar/code/guix-channel/zaijab/systems/pinephone.scm --no-grafts` \
	of=/dev/sda bs=1M oflag=direct,sync status=progress

pinephone-qemu:
	guix shell qemu -- \
	qemu-system-aarch64 \
		-m 1024 \
		-machine orangepi-pc \
		-drive if=none,file=/tmp/my-image.qcow2,id=myhd,format=raw

pinephone-vm:
	sudo $(shell guix system vm zaijab/systems/pinephone.scm --network --share=/home/zjabbar --share=/run/user/1000)
