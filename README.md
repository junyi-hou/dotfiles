# Dotfile

This repository contains my personal configuration for using `home-manager` and `emacs`.

# Usage

## initialization

Make sure the `nix` package manager, `home-manager` and `emacs` are available. The current `$USER` will also need to be a trusted user of `nix`. To install this configuration, put the repository in `~/.config/nixpkgs` and run

```
home-manager switch
emacs --batch -Q main.org -f org-babel-tangle
```

## update

```
git pull --ff
emacs --batch -Q main.org -f org-babel-tangle
```

# License

GPL
