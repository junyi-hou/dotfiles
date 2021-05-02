inputs: with inputs; {
  nixpkgs.overlays = [ emacs-overlay.overlay ];

  home = {
    file.".emacs.d" = {
      source = ../emacs.d;
      recursive = true;
    };
    sessionVariables = {
      EDITOR = "emacsclient -c";
      VISUAL = "emacsclient -c";
    };
  };

  services.emacs = {
    enable = true;
    client.enable = true;
  };
}
