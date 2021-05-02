inputs: with inputs; { lib, ... }: {
  home.activation = {
    initCalDir = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD mkdir -p $VERBOSE_ARG $HOME/.cal
      '';
  };
}
