direnv = {
  nixpkgs.overlays = [
    (final: prev: {
      nix-direnv = prev.nix-direnv.overrideAttrs (oldAttrs: rec {
        # Substitute instead of wrapping because the resulting file is getting sourced, not executed:
        postPatch = ''
            substituteInPlace direnvrc \
              --replace "\''${NIX_BIN_PREFIX}" "\''${NIX_BIN_PREFIX:-${prev.nix}/bin/}" \
              --replace "grep" "${prev.gnugrep}/bin/grep"
          '';
      });
    })
  ];

  programs.direnv = {
    enable = true;
    enableNixDirenvIntegration = true;
    enableBashIntegration = true;
    config = {
      global = {
        strict_env = true;
      };
    };
  };
}
