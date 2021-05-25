{
  description = "Junyi's configuration file";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pragmata-pro = {
      url = "file:///home/junyi/Downloads/PragmataPro0.829.tar.gz";
      flake = false;
    };
    gmail-oauth2-tools = {
      url = "github:google/gmail-oauth2-tools";
      flake = false;
    };
    
    oauth2-lib = {
      url = "github:robn/sasl2-oauth";
      flake = false;
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    # stata = {
    #   url = "file:///home/lactaid/Downloads/stata";
    #   flake = false;
    # };
    rnix-lsp = {
      url = "github:nix-community/rnix-lsp";
    };
    digestif = {
      url = "github:astoff/digestif";
      flake = false;
    };
    nix-gl = {
      url = "github:guibou/nixGL";
      flake = false;
    };
    qmk-firmwork-loader = {
      url = "github:Massdrop/mdloader";
      flake = false;
    };
  };

  outputs = { self, ... }@inputs: {

    supportedPlatforms = {
      "x86_64-linux" = true;
      "x86_64-darwin" = true;
    };

    defaultPackage = builtins.mapAttrs (system: _: self.homeConfigurations."${system}".activationPackage) self.supportedPlatforms;

    realUser = "Junyi Hou";

    homeModules = let
      moduleDir = ./modules;
      importFn = name: _: import (moduleDir + "/${name}") inputs;
      filterAttrs = inputs.nixpkgs.lib.filterAttrs;
      filterFn = name: _: inputs.nixpkgs.lib.hasSuffix ".nix" name;
    in
      builtins.mapAttrs importFn (filterAttrs filterFn (builtins.readDir moduleDir));

    homeConfigurations = with inputs; {
      "x86_64-linux" = home-manager.lib.homeManagerConfiguration rec {
        system = "x86_64-linux";
        homeDirectory = "/home/junyi";
        username = "junyi";
        configuration = { pkgs, lib, config, ... }: {
          programs.home-manager.enable = true;
          imports = [
            {
              home = {
                file = {
                  ".config/i3/config".source = ./linux/i3;
                  ".config/dunst/dunstrc".source = ./linux/dunst;
                };
                packages = [
                  pkgs.libnotify
                  pkgs.dunst
                ];
              };
            }
            {
              home.sessionPath = [ "/usr/bin" ];
            }
            {
              programs.emacs = {
                enable = true;
                package = pkgs.emacsGit;
              };
            }
            # {
            #   home.packages = [
            #     (
            #       let
            #         STATA = pkgs.stdenv.mkDerivation {
            #           name = "stata-14";
            #           src = stata;
            #           # FIXME: still complaining not finding the following libraries
            #           # installing from arch for now
            #           # (libpng12; ncurses5-compat-libs)
            #           propagatedBuildInputs = with pkgs; [ ncurses5 libpng12 ];
            #           installPhase = ''
            #             export plat=linux.64
            
            #             buildDir=$(pwd)
            #             mkdir -p $out
            #             cp unix/linux.64/ado.taz $out/ado.tar.Z
            #             cp unix/linux.64/base.taz $out/base.tar.Z
            #             cp unix/linux.64/bins.taz $out/bins.tar.Z
            #             cp unix/linux.64/docs.taz $out/docs.tar.Z
            #             cp unix/linux.64/setrwxp $out/setrwxp
            #             cp unix/linux.64/inst2 $out/inst2
            
            #             cd $out
            #             ./inst2 now
            
            #             # install license file
            #             cp $buildDir/stata.lic ./
            #           '';
            #         };
            #       in
            #         (
            #           pkgs.writeShellScriptBin "stata-bin" ''
            #             ${STATA}/stata-mp
            #           ''
            #         )
            #     )
            #   ];
            # }
            {
              home.packages = [
                pkgs.xdotool
                pkgs.jq
                (pkgs.writeScriptBin "zathuraInverseSearch" ''
                  #!${pkgs.stdenv.shell}
                  focused_window=$(i3-msg -t get_tree | jq -r 'recurse(.nodes[]) | select( .focused == true)')
                  left=$(echo $focused_window | jq .rect.x)
                  top=$(echo $focused_window | jq .rect.y)
                  width=$(echo $focused_window | jq .window_rect.width)
                  height=$(echo $focused_window | jq .window_rect.height)
            
                  # move cursor to (left+width/2, top+height/2)
                  xdotool mousemove $(expr $left + $width / 2) $(expr $top + $height / 2)
            
                  # send ctrl+click
                  xdotool keydown ctrl click 1 keyup ctrl
                '')
              ];
            }
            {
              home.packages = [ pkgs.firefox ];
            }
            (
              let
                nixGL = (import "${nix-gl}" { pkgs = pkgs; }).nixGLIntel;
                wrapped = pkgs.writeScriptBin "alacritty" ''
                  #!${pkgs.stdenv.shell}
                  exec ${nixGL}/bin/nixGLIntel ${nixpkgs.legacyPackages."${system}".alacritty}/bin/alacritty
                '';
              in
              {
                nixpkgs.overlays = [ (final: prev: { alacritty = wrapped; }) ];
              }
            )
            self.homeModules."font.nix"
            self.homeModules."tree-sitter.nix"
            self.homeModules."grammar.nix"
            self.homeModules."gpg.nix"
            (self.homeModules."pass.nix" homeDirectory)
            self.homeModules."ssh.nix"
            self.homeModules."git.nix"
            self.homeModules."direnv.nix"
            (self.homeModules."mail.nix" homeDirectory)
            self.homeModules."calendar.nix"
            self.homeModules."emacs.nix"
            (self.homeModules."nix-development.nix" system)
            self.homeModules."latex.nix"
            self.homeModules."cli.nix"
            self.homeModules."terminal-emulator.nix"
            self.homeModules."drop-keyboard-loader.nix"
          ];
        };
      };

      "x86_64-darwin" = home-manager.lib.homeManagerConfiguration rec {
        system = "x86_64-darwin";
        homeDirectory = "/Users/lactaid";
        username = "lactaid";
        configuration = { pkgs, lib, config, ... }: {
          programs.home-manager.enable = true;
          imports = [
            (
              let
                libPath = with pkgs; lib.concatStringsSep ":" [
                  "${lib.getLib libgccjit}/lib/gcc/${stdenv.targetPlatform.config}/${libgccjit.version}"
                  "${lib.getLib stdenv.cc.cc}/lib"
                  "${lib.getLib stdenv.glibc}/lib"
                ];
            
                emacsNsGcc = pkgs.emacsGit.overrideAttr (
                  old: {
                    configureFlags = (lib.remove "--with-xft" old.configureFlags)
                      ++ [ "--with-ns" "--with-native-compilation" ];
                    postInstall = old.postInstall or "" + ''
                      ln -snf $out/lib/emacs/28.0.50/native-lisp $out/native-lisp
                      ln -snf $out/lib/emacs/28.0.50/native-lisp $out/Applications/Emacs.app/Contents/native-lisp
                      cat <<EOF> $out/bin/run-emacs.sh
                      #!/usr/bin/env bash
                      set -e
                      exec $out/bin/emacs-28.0.50 "\$@"
                      EOF
                      chmod a+x $out/bin/run-emacs.sh
                      ln -snf ./run-emacs.sh $out/bin/emacs
                    '';
                  }
                );
            
              in
            
              {
                programs.emacs = {
                  enable = true;
                  package = (
                    pkgs.symlinkJoin {
                      name = "emacsGccDarwin";
                      paths = [ emacsNsGcc ];
                      buildInputs = [ pkgs.makeWrapper ];
                      postBuild = ''
                        wrapProgram $out/bin/emacs \
                        --set LIBRARY_PATH ${libPath}
                      '';
                      meta.platforms = pkgs.lib.platforms.darwin;
                      passthru.nativeComp = true;
                      src = emacsNsGcc.src;
                    }
                  );
                };
              }
            )
            self.homeModules."font.nix"
            self.homeModules."tree-sitter.nix"
            self.homeModules."grammar.nix"
            self.homeModules."gpg.nix"
            (self.homeModules."pass.nix" homeDirectory)
            self.homeModules."ssh.nix"
            self.homeModules."git.nix"
            self.homeModules."direnv.nix"
            (self.homeModules."mail.nix" homeDirectory)
            self.homeModules."calendar.nix"
            self.homeModules."emacs.nix"
            (self.homeModules."nix-development.nix" system)
            self.homeModules."latex.nix"
            self.homeModules."cli.nix"
            self.homeModules."terminal-emulator.nix"
            self.homeModules."drop-keyboard-loader.nix"
          ];
        };
      };
    };
  };
}
