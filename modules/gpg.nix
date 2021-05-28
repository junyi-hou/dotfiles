inputs: with inputs; { lib, ... }: {
  programs = {
    gpg = {
      enable = true;
      settings.use-agent = true;
    };
  };

  home = {
    sessionVariables.SSH_AUTH_SOCK = "$(gpgconf --list-dirs agent-ssh-socket)";
    activation = {
      setGpgPermission = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD find ~/.gnupg -type f $VERBOSE_ARG -exec chmod 600 {} \;
        $DRY_RUN_CMD find ~/.gnupg -type d $VERBOSE_ARG -exec chmod 700 {} \;
      '';
    };
  };

  services = {
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 720000; # 200 hours
      maxCacheTtl = 720000;
      defaultCacheTtlSsh = 720000;
      maxCacheTtlSsh = 720000;
      extraConfig = ''
        allow-loopback-pinentry
        allow-emacs-pinentry
      '';
    };
  };
}
