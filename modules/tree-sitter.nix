inputs: with inputs; { pkgs, ... }:
let
  tree-sitter = pkgs.stdenv.mkDerivation {
    pname = "tree-sitter";
    version = "0.19.4";
    src = builtins.fetchurl {
      url = "https://github.com/tree-sitter/tree-sitter/releases/download/v0.19.4/tree-sitter-linux-x64.gz";
      sha256 = "0z4l8sl89z1n4p0xjd89sd7lvs957hfzhsyirrkidx5ycr7dfj30";
    };
    unpackPhase = ''
      cp $src tree-sitter.gz
      gzip -df tree-sitter.gz
    '';
    installPhase = ''
      mkdir -p $out/bin
      install -t $out/bin *
    '';
  };
in
{
  home.packages = [ tree-sitter pkgs.nodejs ];
}
