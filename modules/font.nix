inputs: with inputs; { pkgs, ... }:
let
  pragmataPro = pkgs.stdenv.mkDerivation {
    pname = "PragmataPro";
    version = "0.829";
    src = pragmata-pro;
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
    fonts = [
      pragmataPro
      pkgs.source-han-sans
      pkgs.dejavu_fonts
      (pkgs.nerdfonts.override { fonts = [ "DejaVuSansMono" ]; })
    ];
    fontconfig.enable = true;
  };

  home.file.".config/fontconfig" = {
    source = ../fontconfig;
    recursive = true;
  };
}
