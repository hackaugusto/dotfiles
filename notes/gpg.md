# Key management

Add subkey

```
gpg --edit-key <keyid>
addkey
save
```

List key:

```
gpg --list-secret-keys --keyid-format 0xlong
```

# OpenKeychain

https://www.openkeychain.org/

# Password manager

To re-encrypt all the passwords:

```
rm .gpg-id
pass init <gpgkeys>
```

Remember to add an exclamation mark after the keyid, e.g. `0xaaaa!`, otherwise a
different subkey may be used.

Check with:

```
for f in **/*.gpg; do
    gpg --pinentry-mode cancel --list-packets $f |& grep pubkey | cut '-d ' -f9 | sort -u | tr '\n' ';';
    echo;
done
```
