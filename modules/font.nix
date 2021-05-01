{ pkgs, ... }:
let
  pragmataPro = pkgs.stdenv.mkDerivation {
    pname = "PragmataPro";
    version = "0.828";
    src = pragmata-pro;
    nativeBuildInputs = [ pkgs.unzip ];
    installPhase = ''
      mkdir -p $out/share/fonts/PragmataPro
      cp *.ttf $out/share/fonts/PragmataPro
    '';
  };
in
{
  imports = [
    (
      { config, pkgs, lib, ... }: with lib;
      let
        cfg = config.fonts;
      in
      {
        options = {
          fonts.fonts = mkOption {
            type = types.listOf types.package;
            default = [ ];
            example = literalExample "[ pkgs.dejavu_fonts ]";
            description = "List of fonts.";
          };
        };
    
        config = {
          home = {
            packages = cfg.fonts ++ (if config.fonts.fontconfig.enable then [ pkgs.fontconfig ] else [ ]);
            file = {
              "${config.xdg.cacheHome}/hm-fonts" =
                let
                  getHash = drv: builtins.elemAt
                    (
                      builtins.match "${builtins.storeDir}/([a-z0-9]{32})-.*.drv" drv.drvPath
                    )
                    0;
                in
                {
                  text = concatMapStringsSep "\n" getHash cfg.fonts;
                  onChange = ''
                    echo "Caching fonts"
                    $DRY_RUN_CMD fc-cache -f
                  '';
                };
            };
          };
        };
      }
    )
  ];

  fonts = {
    fonts = [ pragmataPro ];
    fontconfig.enable = true;
  };
}
