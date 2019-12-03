New system
----------

This settings will use full-disk encryption. Both the Boot partiiton and LVM
table will be contained and encrypted by LUKS.

These are the settings for this recipe:
 
```
user=hack
device=/dev/sda
efipartition=/dev/sda1
encryptedpartition=/dev/sda3
lvmname=lvm
lvmvolname=vol
volpartition=root
```

Do the base installation:

https://cryptsetup-team.pages.debian.net/cryptsetup/encrypted-boot.html

```
# if necessary add the partition
cfdisk $device

# luks1 is necessary because Grub doesn't work with luks2
cryptsetup luksFormat $encryptedpartition
cryptsetup open --type luks1 $encryptedpartition $lvmname

pvcreate /dev/mapper/$lvmname
vgcreate vol /dev/mapper/$lvmname
lvcreate -l +100%FREE $lvmvolname -n $volpartition

mkfs.ext4 /dev/mapper/$lvmvolname-$volpartition
mount /dev/mapper/$lvmvolname-$volpartition /mnt

pacstrap /mnt base base-devel grub efibootmgr lvm2 dhcp wpa_supplicant netctl
arch-chroot /mnt
```

Configure Grub (/etc/default/grub):

```
GRUB_CMDLINE_LINUX="cryptdevice=$encryptedpartition:$lvmname"
GRUB_ENABLE_CRYPTODISK=y
GRUB_PRELOAD_MODULES="part_gpt part_msdos cryptodisk luks"
```

```
grub-mkconfig -o /boot/grub/grub.cfg
grub-install target=x86_64-efi --efi-directory=$efipartition --bootloader-id=grub
```

Configure the system:

```
timedatectl set-ntp true
systemctl enable docker.socket
gpasswd -a $user docker
localectl set-locale en_us.utf8
# localectl set-x11-keymap br,us abnt2,pc105 ,dvorak terminate:ctrl_alt_bksp,grp:rctrl_toggle,ctrl:nocaps,ctrl:lctrl_meta
# setxkbmap -layout br,us -model abnt2,pc105 -variant ,dvorak -option terminate:ctrl_alt_bksp,grp:alt_shift_toggle
localectl set-x11-keymap us asus_laptop '' ctrl:swapcaps
```

```
systemctl --user enable gpg-agent-ssh.socket
systemctl --user enable dirmngr.socket
# add the key to the gpg-agent
ssh-keygen -t rsa -b 4096 -C "$(whoami)@$(hostname)-$(date -I)"
ssh-add
```

Configure the initcpio image (/etc/mkinitcpio.conf):

```
Add `encrypt lvm2 consolefont` hooks to /etc/mkinitcpio.conf
Add `intel_agp i915` modules to /etc/mkinitcpio.conf
```

```
mkinitcpio -P
```
