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

# Repository Structure

+ project-root
  - main.org        -- the org file that generates all configuration
  - emacs.d
    - debug-init.el -- the minimal init file for debug purpose
    - etc
      - eshell      -- eshell alias
      - yasnippet   -- snippet files
  - linux
    - fontconfig    -- ~fontconfig~ files
    - sway          -- ~sway~ configuration file
    - waybar        -- ~waybar~ configuration file
    - waybar-style  -- ~waybar~ CSS style file
    - wallpaper.jpg -- wallpaper

# License

GPL
