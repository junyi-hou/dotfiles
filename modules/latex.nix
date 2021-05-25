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
    (
      pkgs.stdenv.mkDerivation {
        pname = "digestif";
        version = "0.3-master";
        src = digestif;
        installPhase = ''
        mkdir -p $out/bin
        cp scripts/digestif $out/bin
      '';
      }
    )
  ];
}
