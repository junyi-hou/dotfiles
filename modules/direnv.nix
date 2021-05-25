inputs: with inputs; {
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
