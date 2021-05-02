inputs: with inputs; { pkgs, ... }: {

  imports = [
    {
      programs.zathura = {
        enable = true;
        options = {
          synctex = true;
          synctex-editor-command = "emacsclient +%{line} %{input}";
        };
        extraConfig = ''
          map <C-r> exec zathuraInverseSearch
        '';
      };
    }
  ];

  home.packages =  [
    pkgs.texlive.combined.scheme-full
    pkgs.texlab
  ];
}
