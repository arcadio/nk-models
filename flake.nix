{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
  };

  outputs = { nixpkgs, ... }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in
      {
        devShell.x86_64-linux = pkgs.mkShell {
          packages = with pkgs; [
            curl
            (rWrapper.override { packages = with rPackages; [ tidyverse broom ggdist MKinfer betareg rstanarm ]; })
          ];
        };
      };
}
