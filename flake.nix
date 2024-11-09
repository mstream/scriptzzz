{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs-firefox-darwin = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:bandithedoge/nixpkgs-firefox-darwin";
    };
    purescript-overlay = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:thomashoneyman/purescript-overlay";
    };
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      nixpkgs-firefox-darwin,
      purescript-overlay,
      ...
    }:
    let
      supportedSystems = [
        "aarch64-darwin"
        "x86_64-linux"
      ];

      conditionalOverlays = [
        {
          overlay = purescript-overlay.overlays.default;
          shouldBeApplied = _: true;
        }
        {
          overlay = nixpkgs-firefox-darwin.overlay;
          shouldBeApplied = system: system == "aarch64-darwin";
        }
      ];

      makeOutput =
        system:
        let
          overlays = builtins.foldl' (
            acc: conditionalOverlay:
            acc ++ (if conditionalOverlay.shouldBeApplied system then [ conditionalOverlay.overlay ] else [ ])
          ) [ ] conditionalOverlays;

          pkgs = import nixpkgs { inherit system overlays; };
        in
        {
          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              deno
              esbuild
              git
              nodejs
              purs
              purs-backend-es
              purs-tidy-bin.purs-tidy-0_11_0
              spago-unstable
            ];
          };
          packages = {
            firefox = pkgs.firefox-bin;
            geckodriver = pkgs.geckodriver;
          };
        };

    in
    flake-utils.lib.eachSystem supportedSystems makeOutput;
}
