{
  description = "using flakes to manage emacs packages";

  inputs = {
    ligature = {
      url = "github:mickeynp/ligature.el";
      flake = false;
    };
    beacon = {
      url = "github:junyi-hou/beacon";
      flake = false;
    };
    org = {
      url = "github:yantar92/org/feature/org-fold-universal-core";
      flake = false;
    };
    org-appear = {
      url = "github:awth13/org-appear/feature/org-fold-support";
      flake = false;
    };
    flymake-childframe = {
      url = "github:junyi-hou/flymake-childframe";
      flake = false;
    };
    tree-sitter-fold = {
      url = "github:junyi-hou/tree-sitter-fold";
      flake = false;
    };
    emacs-calfw = {
      url = "github:tumashu/emacs-calfw";
      flake = false;
    };
    stata-mode = {
      url = "github:junyi-hou/stata-mode";
      flake = false;
    };
  };

  outputs = { self, ... }@inputs: with inputs; {
    emacsModule = emacsPkg: { pkgs, lib, config, ... }: {

      home = {
        file = {
          ".emacs.d/etc" = {
            source = ./etc;
            recursive = true;
          };
          ".emacs.d/init.el".source = ./init.el;
        };
        sessionVariables = {
          EDITOR = "emacsclient -c";
          VISUAL = "emacsclient -c";
        };
      };

      programs.emacs = {
        enable = true;
        package = emacsPkg.pkgs.withPackages (epkgs: [
          epkgs.use-package
          epkgs.dash epkgs.s epkgs.f
          epkgs.gcmh
          epkgs.general
          epkgs.no-littering
          epkgs.visual-fill-column
          epkgs.alert
          epkgs.gruvbox-theme
          (emacsPkg.pkgs.trivialBuild {
            pname   = "ligature";
            version = "9999";
            src = ligature;
          })
          epkgs.highlight-indent-guides
          (emacsPkg.pkgs.trivialBuild {
            pname   = "beacon";
            version = "master";
            src = beacon;
          })
          epkgs.hl-todo
          epkgs.eldoc-box
          epkgs.evil
          epkgs.evil-surround
          epkgs.evil-nerd-commenter
          epkgs.expand-region
          epkgs.selectrum
          epkgs.prescient
          epkgs.selectrum-prescient
          epkgs.consult
          epkgs.marginalia
          (emacsPkg.pkgs.melpaBuild {
            pname = "org";
            ename = "org";
            version = "9999";
            recipe = builtins.toFile "recipe" ''
              (org :fetcher github
                    :repo "yantar92/org"
                    :branch "feature/org-fold-universal-core"
                    :files ("*.el" "lisp/*.el" "contrib/lisp/*.el"))
            '';
            src = org;
          })
          (emacsPkg.pkgs.trivialBuild {
            pname   = "org-appear";
            version = "master";
            src = org-appear;
          })
          epkgs.ob-async
          epkgs.flymake
          (emacsPkg.pkgs.trivialBuild {
            pname   = "flymake-childframe";
            version = "9999";
            src = flymake-childframe;
          })
          epkgs.company
          epkgs.company-prescient
          epkgs.yasnippet
          epkgs.eglot
          epkgs.tree-sitter
          epkgs.tree-sitter-langs
          (emacsPkg.pkgs.trivialBuild {
            pname   = "tree-sitter-fold";
            version = "9999";
            src = tree-sitter-fold;
          })
          epkgs.jupyter
          epkgs.ein
          epkgs.flyspell-correct
          epkgs.langtool
          epkgs.password-store
          epkgs.sudo-edit
          epkgs.magit
          epkgs.magit-delta
          epkgs.forge
          epkgs.projectile
          epkgs.deadgrep
          epkgs.envrc
          epkgs.dired-rainbow epkgs.dired-collapse
          epkgs.notmuch
          epkgs.org-gcal
          (emacsPkg.pkgs.trivialBuild {
            pname = "emacs-calfw";
            version = "9999";
            src = emacs-calfw;
          
            # remove howm backend support
            preBuild = ''
              rm calfw-howm.el
            '';
          })
          epkgs.slack
          epkgs.helpful
          epkgs.aggressive-indent
          epkgs.easy-escape
          (emacsPkg.pkgs.trivialBuild {
            pname = "stata-mode";
            version = "9999";
            src = stata-mode;
          })
          epkgs.ess
          epkgs.nix-mode
          epkgs.markdown-mode
          epkgs.auctex
        ]);
      };

      services.emacs = {
        enable = true;
        client.enable = true;
      };
    };
  };
}
