# Dotfile

This repository contains my personal configuration for using `home-manager` and `emacs`.

# Usage

Make sure the `nix` package manager, `home-manager` and `emacs` are available. The current `$USER` will also need to be a trusted user of `nix`. To install this configuration, run

```
emacs -Q --batch main.org -f org-babel-tangle
home-manager switch
```

# License

GPL
