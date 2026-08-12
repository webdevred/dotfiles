# ZFS on Root with Native Encryption and a LUKS Keystore

Root on ZFS with native encryption, where the encryption key lives in a LUKS container
on a zvol inside the pool itself. You type one passphrase at boot to open the LUKS
container, and the raw key inside it unlocks the pool.

## Status of this document

The reference machine runs Ubuntu 25.10 with ZFS 2.3.4, but it was installed by the
Ubuntu installer, not by following this guide. Everything below was reconstructed by
reading the live system (pool properties, dataset layout, encryption roots, the
initramfs script). The values are therefore accurate, but the procedure as a whole has
not been run start to finish on a blank disk.

Part 1 is plain `sgdisk`, `zpool` and `cryptsetup`, and behaves the same anywhere.
Part 2 is where distributions differ, and only the Debian family is written up. Alpine
and Gentoo have open questions listed rather than answers, because guessing at them is
how you end up with a machine that will not boot.

## Architecture

```
nvme0n1p1  (1G)   EFI (vfat)
nvme0n1p2  (2G)   bpool, unencrypted ZFS, holds /boot so GRUB can read it
nvme0n1p3  (4G)   swap, optional, see "Swap" below
nvme0n1p4  (rest) rpool, ZFS native encryption (aes-256-gcm)
```

### Key chain

```
rpool/keystore  (20M zvol, encryption=off inside an otherwise encrypted rpool)
  └── /dev/zvol/rpool/keystore
       └── LUKS2 container  ← the passphrase you type at boot
            └── ext4 at /run/keystore/rpool/
                 └── system.key  ← 32-byte raw key that unlocks rpool
```

A dataset with `encryption=off` under an encrypted parent is allowed. The keystore zvol
on the reference machine reports `encryption off` and `encryptionroot -` while its
parent `rpool` is `aes-256-gcm`. This is what makes the whole scheme possible.

`rpool` is the only encryption root. Every dataset below it inherits the key and has
`keylocation none`; only `rpool` itself carries the `keylocation` pointing at the
keystore.

### How the keystore is unlocked at boot

On the reference machine this is done by `/usr/share/initramfs-tools/scripts/zfs`,
shipped in the `zfs-initramfs` package. Around line 1026 it:

1. Lists datasets whose name ends in `/keystore`.
2. Waits (5s timeout) for the matching device to appear under `/dev/zvol/`.
3. Writes a crypttab line `keystore-<pool> <device> none luks,discard` into a temporary
   tabfile.
4. Opens it with cryptsetup, which is what prompts for the passphrase.
5. Mounts the ext4 filesystem at `/run/keystore/<pool>/`.
6. Loads the ZFS key from `keylocation`.

This is why `/etc/crypttab` contains no keystore entry. The entry is generated on every
boot. Nothing needs to be added there by hand.

---

# Part 1: distribution-independent

## Step 1: Partition the disk

```bash
DISK=/dev/nvme0n1

sgdisk --zap-all "$DISK"

sgdisk -n 1:0:+1G -t 1:EF00 -c 1:EFI   "$DISK"
sgdisk -n 2:0:+2G -t 2:BF01 -c 2:bpool "$DISK"
sgdisk -n 3:0:+4G -t 3:8200 -c 3:swap  "$DISK"
sgdisk -n 4:0:0   -t 4:BF00 -c 4:rpool "$DISK"

partprobe "$DISK"
mkfs.vfat -F 32 -n EFI "${DISK}p1"
```

Find the stable device path to use for the pools. Pools created from `/dev/nvme0n1pN`
can fail to import cleanly after a reboot, so use `by-id`:

```bash
ls -l /dev/disk/by-id/ | grep nvme
```

Pick the entry for your disk and note the `-partN` suffixes. The rest of this guide
writes it as `$DISKID`:

```bash
DISKID=/dev/disk/by-id/nvme-SAMSUNG_MZ...   # your value here
```

## Step 2: Create bpool

GRUB understands only a subset of ZFS features, so bpool is created with all features
off and the readable ones switched back on individually. The reference machine has
exactly these eleven enabled:

```bash
zpool create \
  -o ashift=12 \
  -o autotrim=on \
  -d \
  -o feature@async_destroy=enabled \
  -o feature@bookmarks=enabled \
  -o feature@embedded_data=enabled \
  -o feature@empty_bpobj=enabled \
  -o feature@enabled_txg=enabled \
  -o feature@extensible_dataset=enabled \
  -o feature@filesystem_limits=enabled \
  -o feature@hole_birth=enabled \
  -o feature@large_blocks=enabled \
  -o feature@lz4_compress=enabled \
  -o feature@spacemap_histogram=enabled \
  -O compression=lz4 \
  -O atime=on \
  -O canmount=off \
  -O mountpoint=/boot \
  -R /mnt \
  bpool "${DISKID}-part2"
```

Newer ZFS also has `-o compatibility=grub2`, which is less typing. It is a different
feature set from the one above, so it produces a pool that does not match the reference
machine. Use it only if you have checked that your GRUB reads it.

Never run `zpool upgrade` on bpool.

## Step 3: Generate the encryption key

```bash
dd if=/dev/urandom bs=32 count=1 of=/tmp/system.key
chmod 400 /tmp/system.key
```

## Step 4: Create rpool

`keylocation` points at the temporary key for now and is repointed in step 7.

```bash
zpool create \
  -o ashift=12 \
  -o autotrim=on \
  -o compatibility=openzfs-2.3 \
  -O encryption=aes-256-gcm \
  -O keylocation=file:///tmp/system.key \
  -O keyformat=raw \
  -O compression=lz4 \
  -O atime=on \
  -O dnodesize=auto \
  -O acltype=posixacl \
  -O xattr=sa \
  -O normalization=formD \
  -O canmount=off \
  -O mountpoint=/ \
  -R /mnt \
  rpool "${DISKID}-part4"
```

## Step 5: Create the keystore zvol

The name must end in `/keystore` for the initramfs script to find it, and
`encryption=off` must be explicit so it does not inherit encryption from rpool.

```bash
zfs create -V 20M -b 16K -o encryption=off rpool/keystore

udevadm settle
ls -l /dev/zvol/rpool/keystore
```

## Step 6: Put LUKS on the keystore

```bash
cryptsetup luksFormat --type luks2 /dev/zvol/rpool/keystore
cryptsetup open /dev/zvol/rpool/keystore keystore-rpool

mkfs.ext4 -L keystore-rpool /dev/mapper/keystore-rpool

mkdir -p /run/keystore/rpool
mount /dev/mapper/keystore-rpool /run/keystore/rpool

cp /tmp/system.key /run/keystore/rpool/system.key
chmod 400 /run/keystore/rpool/system.key
shred -u /tmp/system.key
```

The mapper name `keystore-rpool` matches what the initramfs script generates, so use it
here too.

## Step 7: Repoint rpool at the keystore

```bash
zfs set keylocation=file:///run/keystore/rpool/system.key rpool
```

## Step 8: Create the datasets

```bash
zfs create -o canmount=off -o mountpoint=none bpool/BOOT
zfs create -o canmount=noauto -o mountpoint=/boot bpool/BOOT/default
zfs mount bpool/BOOT/default

zfs create -o canmount=off -o mountpoint=none rpool/ROOT
zfs create -o canmount=noauto -o mountpoint=/ rpool/ROOT/default
zfs mount rpool/ROOT/default

zfs create rpool/ROOT/default/srv
zfs create -o canmount=off rpool/ROOT/default/usr
zfs create rpool/ROOT/default/usr/local
zfs create -o canmount=off rpool/ROOT/default/var
zfs create rpool/ROOT/default/var/games
zfs create -o canmount=off rpool/ROOT/default/var/lib
zfs create rpool/ROOT/default/var/lib/AccountsService
zfs create rpool/ROOT/default/var/lib/NetworkManager
zfs create rpool/ROOT/default/var/log
zfs create rpool/ROOT/default/var/mail
zfs create rpool/ROOT/default/var/spool
zfs create rpool/ROOT/default/var/www

zfs create -o canmount=off -o mountpoint=none rpool/USERDATA
zfs create -o mountpoint=/home rpool/USERDATA/home
zfs create -o mountpoint=/root rpool/USERDATA/root
```

The reference machine names these `rpool/ROOT/ubuntu_mby1w7` and
`rpool/USERDATA/home_ew2mea`. The random suffixes are a zsys convention from the Ubuntu
installer and serve no purpose in a manual install, so `default` is used here instead.

### Data that should survive a rollback

The reference machine keeps Docker outside the `ROOT` hierarchy, so snapshots and
rollbacks of the boot environment do not touch container layers:

```bash
zfs create -o canmount=off rpool/var
zfs create -o canmount=off rpool/var/lib
zfs create rpool/var/lib/docker
```

> TODO: the exact `canmount` and `mountpoint` values on `rpool/var` and `rpool/var/lib`
> were not read off the reference machine. `canmount=off` above is inference, not
> measurement. Confirm with
> `zfs list -o name,canmount,mountpoint rpool/var rpool/var/lib rpool/var/lib/docker`
> before trusting this block. The two parent datasets collide by mountpoint with
> `rpool/ROOT/default/var` and `/var/lib`, so getting `canmount` wrong here means two
> datasets fighting over the same mountpoint.

---

# Part 2: distribution-specific

## Debian family

Closest to the reference machine, since it is the same `zfs-linux` source package and
the same `initramfs-tools`.

Confirm at first install, do not assume:

- ZFS lives in `contrib`, not `main`, and is built through DKMS rather than shipped
  prebuilt. The live environment needs the module before it can create pools.
- Whether Debian's build of `zfs-initramfs` carries the same keystore block described
  above. If it does not, the machine will not unlock at boot and the block has to be
  supplied as a local initramfs script. Check with
  `grep -n keystore /usr/share/initramfs-tools/scripts/zfs` in the live environment.

Bootstrap:

```bash
debootstrap --arch=amd64 <suite> /mnt

mount -t proc     proc     /mnt/proc
mount -t sysfs    sysfs    /mnt/sys
mount -t devtmpfs devtmpfs /mnt/dev
mount -t devpts   devpts   /mnt/dev/pts
mount -t tmpfs    tmpfs    /mnt/run
mkdir -p /mnt/run/lock

mkdir -p /mnt/boot/efi
mount "${DISK}p1" /mnt/boot/efi

mkdir -p /mnt/run/keystore/rpool
mount --bind /run/keystore/rpool /mnt/run/keystore/rpool

chroot /mnt /bin/bash
```

Inside the chroot, enable `contrib` in the apt sources before installing, then:

```bash
apt-get update
apt-get install --yes \
  linux-image-amd64 linux-headers-amd64 \
  zfs-dkms zfsutils-linux zfs-initramfs \
  grub-efi-amd64 shim-signed \
  cryptsetup cryptsetup-initramfs
```

### fstab

```bash
EFI_UUID=$(blkid -s UUID -o value "${DISK}p1")
echo "UUID=${EFI_UUID} /boot/efi vfat umask=0077 0 1" > /etc/fstab
```

ZFS datasets are not listed in fstab. They are mounted from the cache described below.

### Mount cache

`zfs-mount-generator` reads `/etc/zfs/zfs-list.cache/<pool>` and turns each line into a
systemd mount unit. One file per pool, containing only that pool's datasets:

```bash
mkdir -p /etc/zfs/zfs-list.cache

PROPS=name,mountpoint,canmount,atime,relatime,devices,exec,setuid,readonly,zoned,org.freebsd:swap,keylocation,com.sun:auto-snapshot,org.openzfs.systemd:requires,org.openzfs.systemd:requires-mounts-for,org.openzfs.systemd:before,org.openzfs.systemd:after,org.openzfs.systemd:wanted-by,org.openzfs.systemd:required-by,encroot

zfs list -H -o "$PROPS" -r bpool > /etc/zfs/zfs-list.cache/bpool
zfs list -H -o "$PROPS" -r rpool > /etc/zfs/zfs-list.cache/rpool

sed -Ei 's|/mnt/?|/|' /etc/zfs/zfs-list.cache/*
```

The `sed` matters. The pools were created with `-R /mnt`, so every mountpoint in the
listing comes out prefixed with `/mnt`. Written to the cache unprefixed, the installed
system tries to mount its root at `/mnt` and fails.

Verify before moving on:

```bash
grep -c . /etc/zfs/zfs-list.cache/rpool   # non-zero
grep /mnt /etc/zfs/zfs-list.cache/*       # no output
```

### GRUB

```bash
echo 'GRUB_CMDLINE_LINUX="root=ZFS=rpool/ROOT/default"' >> /etc/default/grub

grub-probe /boot     # must print "zfs"

update-initramfs -c -k all
grub-install --target=x86_64-efi --efi-directory=/boot/efi \
  --bootloader-id=debian --recheck --no-floppy
grub-mkconfig -o /boot/grub/grub.cfg
```

`update-grub` is a Debian wrapper around `grub-mkconfig`. The explicit form is used here
so the command carries over to distributions that do not ship the wrapper.

Set a root password with `passwd`, then leave the chroot.

## Alpine

Not written. Questions to answer while installing the first machine, in this order:

1. Which initramfs generator is in use, and is there ZFS support for it?
2. Does that support contain the keystore logic (find `*/keystore`, open the LUKS
   container, mount it, load the key), or does it have to be written by hand?
3. Alpine does not use systemd, so `zfs-mount-generator` and the `zfs-list.cache`
   mechanism above do not apply. How are datasets mounted after the pool is imported?
4. Does the `initramfs` option in crypttab exist, or is that specific to
   `initramfs-tools`?
5. Which bootloader, and can it read bpool with the feature set from step 2?
6. Package names for the ZFS userland and kernel module.

If the answer to question 2 is no, the alternative worth costing out is moving the
keystore off the zvol and onto its own small partition. It then becomes an ordinary LUKS
device that any initramfs can open before the pool is imported, and the ordering problem
disappears. The security properties are the same, the key is LUKS-protected either way.

## Gentoo

Not written. Same questions as Alpine, plus which of the available initramfs generators
you intend to use, since that choice decides everything in question 2 and 3.

---

## Swap

The reference machine has the swap partition but no active swap: both the `/etc/crypttab`
line and the fstab line are commented out. Skip this section to match it.

To actually enable encrypted swap, both halves must be uncommented together. Enabling
only the fstab side leaves the system waiting at boot for a mapper device that is never
created.

```bash
SWAP_UUID=$(blkid -s UUID -o value "${DISK}p3")

echo "dm_crypt-0 UUID=${SWAP_UUID} /dev/urandom swap,initramfs,size=256,cipher=aes-cbc-essiv:sha256,hash=sha256" \
  >> /etc/crypttab

echo "/dev/mapper/dm_crypt-0 none swap sw 0 0" >> /etc/fstab
```

The `initramfs` option is an `initramfs-tools` extension. On other initramfs generators
it is either ignored or an error.

Swap on a zvol is a separate option and is deliberately not covered here. It has a known
deadlock under memory pressure.

## Finishing up

```bash
exit
umount /mnt/run/keystore/rpool
umount /mnt/boot/efi
umount /mnt/run
umount /mnt/dev/pts /mnt/dev /mnt/proc /mnt/sys
zfs unmount -a
zpool export bpool
zpool export rpool
cryptsetup close keystore-rpool
```

Reboot and remove the installation media.

## First boot

1. GRUB reads the kernel from bpool, which is unencrypted.
2. The initramfs imports rpool. Pool metadata is readable without the key.
3. `rpool/keystore` appears under `/dev/zvol/`.
4. The initramfs script opens the LUKS container and prompts for the passphrase.
5. The ext4 filesystem is mounted at `/run/keystore/rpool/`.
6. `system.key` unlocks rpool and the remaining datasets mount.

If it stops at an initramfs prompt, the useful checks are `zpool status` (did the import
happen), `ls /dev/zvol/rpool` (is the keystore visible) and `ls /run/keystore/rpool` (did
the mount happen). Which one fails tells you which step above to look at.

## Notes

- The passphrase you type at boot is the LUKS passphrase on the keystore. The ZFS key
  itself never leaves the machine.
- Add a recovery keyslot with
  `cryptsetup luksAddKey /dev/zvol/rpool/keystore`. Do this before you need it.
- Back up the LUKS header. Losing it loses the pool, even though the ZFS key is
  intact: `cryptsetup luksHeaderBackup /dev/zvol/rpool/keystore --header-backup-file <path>`.
  Store it somewhere that is not this machine.
- Verify a finished install with
  `zfs get encryption,encryptionroot,keylocation,keyformat rpool`. It should report
  `aes-256-gcm`, `rpool`, the keystore path, and `raw`.
- bpool must stay unencrypted. GRUB cannot read ZFS native encryption.
- Do not enable features on bpool beyond the eleven in step 2.
