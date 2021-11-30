# Arch Server Install Script

Utility to install archlinux on a bare-metal server on Hetzner.

## Disk

Setup geared torwards SDD

- Full disk encryption with dmcrypt/LUKS (safer / reboot requires password)
- Raid0 (faster / no data recovery)
- XFS (faster / can't shrink)

## System

- Remote unlocking during boot is done with dropbear

## Missing

- Network configuration for subnet communicatoin [gateway].

[gateway]: https://docs.hetzner.com/robot/dedicated-server/network/network-configuration-using-systemd-networkd

# References

- https://github.com/kevinveenbirkenbach/hetzner-arch-luks
- https://github.com/hetzneronline/installimage
- https://www.phoronix.com/scan.php?page=article&item=linux-58-filesystems

## Notes


### TRIM

- enabled on dmcrypt/LUKS (no-default setting)
- disabled in the file system (default for XFS)
- dispatched weekly using fstrim.timer
