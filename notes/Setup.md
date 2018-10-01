New system
----------

```
user=hack
esp_mount=/dev/sda1
target_partition=/dev/sda3
mapping_name=lvm
volume_name=vol
volume_partition=root
```

Grub config (/etc/default/grub):
```
GRUB_CMDLINE_LINUX="cryptdevice=$target_partition:$mapping_name"
GRUB_ENABLE_CRYPTODISK=y
GRUB_PRELOAD_MODULES="part_gpt part_msdos cryptodisk luks"
```

/etc/mkinitcpio.conf config:

Add `encrypt lvm2 consolefont` hooks to /etc/mkinitcpio.conf
Add `intel_agp i915` modules to /etc/mkinitcpio.conf

Change `/etc/locale.conf`


```
gdisk /dev/sda

cryptsetup luksFormat $target_partition
cryptsetup open --type luks $target_partition $mapping_name
pvcreate /dev/mapper/$mapping_name
vgcreate vol /dev/mapper/$mapping_name

lvcreate -l +100%FREE $volume_name -n $volume_partition
mkfs.ext4 /dev/mapper/$volume_name-$volume_partition
mount /dev/mapper/$volume_name-$volume_partition /mnt

pacstrap /mnt base
arch-chroot /mnt
pacman -S grub efibootmgr base-devel
mkinitcpio -p linux

grub-mkconfig -o /boot/grub/grub.cfg
grub-install target=x86_64-efi --efi-directory=$esp_mount --bootloader-id=grub

timedatectl set-ntp true

systemctl enable docker.socket
gpasswd -a $user docker
localectl set-locale en_us.utf8

# localectl set-x11-keymap br,us abnt2,pc105 ,dvorak terminate:ctrl_alt_bksp,grp:rctrl_toggle,ctrl:nocaps,ctrl:lctrl_meta
localectl set-x11-keymap us asus_laptop '' ctrl:swapcaps

systemctl --user enable gpg-agent-ssh.socket
systemctl --user enable dirmngr.socket

# add the key to the gpg-agent
ssh-keygen -t rsa -b 4096 -C "$(whoami)@$(hostname)-$(date -I)"
ssh-add
```
