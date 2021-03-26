# Dotfile

This repository contains my personal configuration for using `home-manager` and `emacs`.

# Usage

## Initialization

Make sure the `nix` package manager, `home-manager` are available. The current `$USER` will also need to be a trusted user of `nix`. To install this configuration, clone this repository. In its root directory, run
```
update-user-config
```

## Update

In command line, run

```
emacs -Q --batch main.org -f org-babel-tangle
```

Or in `emacs`, run

```
M-x find-file ./main.org
M-x org-babel-tangle
```

# License

GPL
