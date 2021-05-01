{
  programs.ssh = {
    enable = true;
    matchBlocks = {
      "aws" = {
        hostname = "18.219.125.103";
        user = "root";
        identityFile = "~/.ssh/aws-key.pem";
      };
    };
  };
}
