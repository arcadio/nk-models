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
            (rWrapper.override { packages = with rPackages; [ broom purrr ggplot2 ggdist MKinfer betareg rstanarm ]; })
          ];
        };
      };
}
