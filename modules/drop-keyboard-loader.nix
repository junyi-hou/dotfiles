inputs: with inputs; { pkgs, ... }: {
  home.packages = [
    (
      pkgs.stdenv.mkDerivation {
        name = "mdloader";
        src = qmk-firmwork-loader;
        installPhase = ''
          install -Dt $out/bin/ ./build/mdloader
          cp applet-mdflash.bin $out/bin/
        '';
      }
    )
  ];
}
