{ pkgs, lib, config, ... }: {
  programs.home-manager.enable = true;
  imports = [
    
    self.homeModules."font.nix"
    self.homeModules."vterm.nix"
    self.homeModules."grammar.nix"
    self.homeModules."gpg.nix"
    (self.homeModules."pass.nix" homeDirectory)
    self.homeModules."direnv.nix"
    (self.homeModules."mail.nix" homeDirectory)
    self.homeModules."calendar.nix"
    (self.homeModules."nix-development.nix" system)
    self.homeModules."latex.nix"
    self.homeModules."cli.nix"
    self.homeModules."emacs.nix"
    self.homeModules."ssh.nix"
    self.homeModules."git.nix"
    self.homeModules."tree-sitter.nix"
    self.homeModules."drop-keyboard-loader.nix"
  ];
}
