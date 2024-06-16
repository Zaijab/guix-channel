image=$(guix system image /home/zjabbar/code/guix-channel/zaijab/systems/pinephone.scm --no-grafts --image-type=rock64-raw)
cp $image /tmp/my-image.qcow2
chmod +w /tmp/my-image.qcow2
qemu-system-aarch64 -enable-kvm -hda /tmp/my-image.qcow2 -m 1000 \
		    -bios $(guix build ovmf-x86-64)/share/firmware/ovmf_x64.bin
