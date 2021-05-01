{ pkgs, lib, config, ... }: {
  programs.home-manager.enable = true;
  imports = [
    {
      home = {
        file = {
          ".config/sway/config".source = ./linux/sway;
          ".config/sway/wallpaper.jpg".source = ./linux/wallpaper.jpg;
          ".config/waybar/config".source = ./linux/waybar;
          ".config/waybar/style.css".source = ./linux/waybar-style;
          ".config/fontconfig" = {
            source = ./linux/fontconfig;
            recursive = true;
          };
        };
      };
    }
    {
      home.sessionPath = [ "/usr/bin" ];
    }
    {
      home.packages = with pkgs; [
        bemenu
        havoc # terminal
      ];
    
      home.file.".config/havoc.cfg" = {
        text = ''
          [child]
          program=bash
          [font]
          size=25
          [bind]
          C-S-c=copy
          C-S-v=paste
        '';
      };
    }
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
