{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-schemas.url = "github:DeterminateSystems/flake-schemas";
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
      flake-schemas,
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
          case = key: options: options."${key}";

          overlays = builtins.foldl' (
            acc: conditionalOverlay:
            acc ++ (if conditionalOverlay.shouldBeApplied system then [ conditionalOverlay.overlay ] else [ ])
          ) [ ] conditionalOverlays;

          pkgs = import nixpkgs { inherit system overlays; };

          wdioBrowserDependencies = {
            # WORKAROUND: dmg distribution of firefox does not work on MacOS
            firefox = pkgs.librewolf;
            geckodriver = pkgs.geckodriver;
          };

          makeWdioBrowserDependencyPath =
            name: package:
            case name {
              firefox = case system {
                aarch64-darwin = "${package}/Applications/LibreWolf.app/Contents/MacOs/librewolf";
                x86_64-linux = "${package}/bin/librewolf";
              };
              geckodriver = "${package}/bin/geckodriver";
            };
        in
        {
          devShells.default = pkgs.mkShell {
            buildInputs =
              with pkgs;
              [
                deno
                esbuild
                git
                nodejs
                purs
                purs-backend-es
                purs-tidy-bin.purs-tidy-0_11_0
                spago-unstable
              ]
              ++ builtins.attrValues wdioBrowserDependencies;
          };
          packages = {
            wdioBrowserDependencyPaths = builtins.mapAttrs makeWdioBrowserDependencyPath wdioBrowserDependencies;
          };
          schemas = flake-schemas.schemas;
        };

    in
    flake-utils.lib.eachSystem supportedSystems makeOutput;
}
