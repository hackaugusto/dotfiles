#!/bin/bash

set -o errexit
set -o pipefail
set -o nounset

readonly -a DISKS=("/dev/nvme0n1" "/dev/nvme1n1")
readonly BOOTSTRAP_FILE="archlinux-bootstrap-2021.11.01-x86_64.tar.gz"
readonly BOOSTRAP_URL="http://arch.yhtez.xyz/iso/2021.11.01/${BOOTSTRAP_FILE}"
readonly BOOTSTRAP_FILE_SIGNATURE_URL="https://archlinux.org/iso/2021.11.01/archlinux-bootstrap-2021.11.01-x86_64.tar.gz.sig"

readonly TZ="Europe/Berlin"
readonly LOCALE="en_US.UTF-8"
readonly HOSTNAME="astro"

readonly PHYSICAL_VOLUME_PREFIX="crypt-"
readonly LOGICAL_VOLUME="root"
readonly LOGICAL_VOLUME_TYPE="raid0"
readonly VOLUME_GROUP="disks"
readonly MAPPER_NAME="${VOLUME_GROUP}-${LOGICAL_VOLUME}"

readonly EXTRACT_DIR="/tmp"
readonly BOOTSTRAP_DIR="${EXTRACT_DIR}/root.x86_64"
readonly BOOTSTRAP_CHROOT="${BOOTSTRAP_DIR}/usr/bin/arch-chroot" "${BOOTSTRAP_DIR}"
readonly INSTALL_DIR="${BOOTSTRAP_DIR}/mnt"
readonly INSTALL_CHROOT="${BOOTSTRAP_DIR}/usr/bin/arch-chroot" "${INSTALL_DIR}"
readonly -a ARCH_DEPENDENCIES=(
    base
    linux linux-firmware
    cronie
    xfsprogs haveged
    grub busybox
    lvm2 mdadm cryptsetup
    net-tools openssh inetutils
    # for remote unlocking
    mkinitcpio-systemd-tool
    # for fstrim
    util-linux
)

# Stop LVM and software RAID
# vgchange -ativate n
# dmsetup remove_all
# mdadm --stop /dev/md*

# [setup servers disk]
# for disk in "${DISKS[@]}"; do
#     hdparm -I ${disk} | grep TRIM
# done
for disk in "${DISKS[@]}"; do
    pv --eta --rate --stop-at-size -s "$(blockdev --getsize64 ${disk})" /dev/urandom > ${disk}
done
for disk in "${DISKS[@]}"; do
    sfdisk "${disk}" < ./partition_table
done
for disk in "${DISKS[@]}"; do
    cryptsetup luksFormat "${disk}p2"
done

set -a physical_volumes
for count in $(seq ${#DISKS[@]}); do
    physical_volumes+=("${PHYSICAL_VOLUME_PREFIX}-${count}")
done
for pv in "${physical_volumes[@]}"; do
    cryptsetup luksOpen --allow-discards "${pv}"
    pvcreate "/dev/mapper/${pv}"
done
vgcreate "${VOLUME_GROUP}" "${physical_volumes[@]}"
lvcreate --type "${LOGICAL_VOLUME_TYPE}" --extends +100%FREE "${VOLUME_GROUP}" --name "${LOGICAL_VOLUME}"

mkfs.xfs -m bigtime=1 "/dev/mapper/${MAPPER_NAME}"
for disk in "${DISKS[@]}"; do
    mkfs.ext4 "/disk/${disk}p1"
done

# [download arch]
wget "${BOOSTRAP_URL}"
# XXX: Request is interceptable, this does not guarantee the file is not malicious
wget "${BOOTSTRAP_FILE_SIGNATURE_URL}"

gpg --keyserver-options auto-key-retrieve --verify "${BOOTSTRAP_FILE}.sig"
gpgv "${BOOTSTRAP_FILE}.sig" "${BOOTSTRAP_FILE}"

# [install arch deps]
tar --numeric-owner --extract --ungzip --file "${BOOTSTRAP_FILE}" --directory "${EXTRACT_DIR}"
mount --bind "${BOOTSTRAP_DIR}" "${BOOTSTRAP_DIR}"
echo 'Server=https://mirror.hetzner.com/archlinux/$repo/os/$arch' > "${BOOTSTRAP_DIR}/etc/pacman.d/mirrorlist"
$BOOTSTRAP_CHROOT pacman-key --init
$BOOTSTRAP_CHROOT pacman-key --populate archlinux
$BOOTSTRAP_CHROOT pacman -Syy
$BOOTSTRAP_CHROOT pacman --noconfirm -S archlinux-keyring

mount "/dev/mapper/${MAPPER_NAME}" "${INSTALL_DIR}"
$BOOTSTRAP_CHROOT pacstrap -d -G -M "${INSTALL_DIR}" "${ARCH_DEPENDENCIES[@]}"

# [config system]
$BOOTSTRAP_CHROOT pacstrap -d -G -M "${INSTALL_DIR}" "${ARCH_DEPENDENCIES[@]}"

# TODO: subnet network
# 30/11/21:
# - hetzner rescue versions: debian 11.0 (bullseye), systemd/udev 247, iproute2 5.9.0
# - predictable interface names is disable with kernel command line net.ifnames=0
# - it doesn't change system'd .link file, which enables alternative naming
# readonly interfaces=($(ip -json link show | jq -r '.[] | select(.link_type == "ether") | .altnames[0]'))
$INSTALL_CHROOT timedatectl set-ntp true
genfstab -U /mnt >> "${INSTALL_DIR}/etc/fstab"

$INSTALL_CHROOT ln -sf "/usr/share/zoneinfo/${TZ}" /etc/localtime
$INSTALL_CHROOT hwclock --systohc

echo "${LOCALE}" > "${INSTALL_DIR}/etc/locale.gen"
echo "${LOCALE}" > "${INSTALL_DIR}/etc/locale.conf"
$INSTALL_CHROOT locale-gen

echo "${HOSTNAME}" > "${INSTALL_DIR}/etc/hostname"

$INSTALL_CHROOT systemctl enable fstrim.timer
$INSTALL_CHROOT systemctl enable initrd-cryptsetup.path
$INSTALL_CHROOT systemctl enable initrd-dropbear.service
$INSTALL_CHROOT systemctl enable initrd-debug-progs.service
$INSTALL_CHROOT systemctl enable initrd-sysroot-mount.service
cp "${HOME}/.ssh/authorized_keys" "${INSTALL_DIR}/etc/dropbear/dropbear_rsa_host_key"

# TODO: base autodetect modconf block filesystems keyboard fsck systemd systemd-tool lvm2
# TODO: /etc/cryptab /etc/mkinitcpio-systemd-tool/config/crypttab /etc/mkinitcpio-systemd-tool/config/fstab
$INSTALL_CHROOT mkinitcpio -P

# TODO:# [install grub]
$INSTALL_CHROOT grub-mkconfig -o /boot/grub/grub.cfg
for disk in "${DISKS[@]}"; do
    $INSTALL_CHROOT grub-install "/${disk}p1"
    mkfs.ext4 
done
