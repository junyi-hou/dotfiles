{
  description = "Junyi's configuration file";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pragmata-pro = {
      url = "file:///home/lactaid/Downloads/PragmataPro0.828-2.zip";
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
    qmk-firmwork-loader = {
      url = "github:Massdrop/mdloader";
      flake = false;
    };
  };

  outputs = { self, ... }@inputs: with inputs; {
    realUser = "Junyi Hou";

    homeModules = with builtins;
      let
        moduleDir = ./modules;
        importFn = name: _: import (moduleDir + "/${name}");
        filterAttrs = nixpkgs.lib.filterAttrs;
        filterFn = name: _: nixpkgs.lib.hasSuffix ".nix" name;
      in
        mapAttrs importFn (filterAttrs filterFn (readDir moduleDir));

    homeConfigurations = {
      "x86_64-linux" = home-manager.lib.homeManagerConfiguration rec {
        system = "x86_64-linux";
        homeDirectory = "/home/lactaid";
        username = "lactaid";
        configuration = ./linux/homeConfig.nix;
      };

      "x86_64-darwin" = home-manager.lib.homeManagerConfiguration rec {
        system = "x86_64-darwin";
        homeDirectory = "/Users/lactaid";
        username = "lactaid";
        configuration = ./darwin/homeConfig.nix;
      };
    };
  };
}
