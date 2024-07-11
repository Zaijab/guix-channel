all: git pull_from_lock system

gc:
	guix gc -d 1m

git:
	git add -A
	git diff-index --quiet HEAD || git commit -am "Updating Config"
	git push -u github main

pull:
	guix pull --channels=/home/zjabbar/code/guix-channel/zaijab/channel.scm --allow-downgrades

pull_from_lock:
	guix pull --channels=/home/zjabbar/code/guix-channel/zaijab/files/channel.tmpl --allow-downgrades

update_lock:
	guix pull --channels=/home/zjabbar/code/guix-channel/zaijab/channel.scm --allow-downgrades
	guix describe --format=channels > /home/zjabbar/code/guix-channel/zaijab/files/channel_lock.tmpl

system:
	sudo guix system reconfigure -e '(@ (zaijab systems based-system) my-operating-system)' --allow-downgrades

print_shepherd_log:
	sudo cat /var/log/messages

init:
	mkdir ~/.mail/zaijab2000

pinephone-build:
	guix system image /home/zjabbar/code/guix-channel/zaijab/systems/pinephone.scm --no-grafts --image-type=rock64-raw -v 4

pinephone-write:
	sudo dd if=`guix system image --image-type=rock64-raw /home/zjabbar/code/guix-channel/zaijab/systems/pinephone.scm --no-grafts` \
	of=/dev/sda bs=1M oflag=direct,sync status=progress

pinephone-qemu:
	image=$(guix system image /home/zjabbar/code/guix-channel/zaijab/systems/pinephone.scm --no-grafts --image-type=rock64-raw)
	cp $image /tmp/my-image.qcow2
	chmod +w /tmp/my-image.qcow2
	guix shell qemu -- \
	qemu-system-aarch64 -enable-kvm -hda /tmp/my-image.qcow2 -m 1000 \
		-bios $(guix build ovmf-x86-64)/share/firmware/ovmf_x64.bin

