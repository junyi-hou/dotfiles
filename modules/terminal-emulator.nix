inputs: with inputs; { pkgs, ... }: {
  programs.alacritty = {
    enable = true;
    settings = {
      font.size = 9;
    };
  };
}
