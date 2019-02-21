---
title: GPG key migration quick guide
---

- Get key id: `gpg --list-secret-keys`
- Export private key: `gpg --export-secret-keys -a keyid > private_key.asc`
- Export public key: `gpg --export -a keyid > public_key.asc`
- Import private key: `gpg --import private_key.asc`
- Trust the key: `gpg --edit-key foo@bar.com`. Type in the command `trust`, then select "I trust ultimately". Use `quit` to quit.

Configure git to use the GPG key, and sign commits automatically:

- `git config --global user.signingkey`
- `git config --global commit.gpgSign true`