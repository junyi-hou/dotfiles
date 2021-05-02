inputs: with inputs; system: { pkgs, ... }: {
  home.packages = [
    rnix-lsp.defaultPackage."${system}"
    pkgs.nixpkgs-fmt
  ];
}
