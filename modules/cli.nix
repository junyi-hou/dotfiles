inputs: with inputs; { pkgs, ... }: {

  imports = [
    {
      home.packages = [ pkgs.bat ];
    }
    {
      home.packages = [ pkgs.fd ];
    }
    {
      home.packages = [ pkgs.ripgrep ];
    }
  ];

  home.packages = [ pkgs.tree pkgs.curl pkgs.htop pkgs.inetutils ];
}
