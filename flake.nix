{
  description = "Behaviour of the logistic map";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "master";
    };
  };

  outputs = { self, nixpkgs }: {

    packages.x86_64-linux.executable =
      with import nixpkgs { system = "x86_64-linux"; };
      let
        source = nix-gitignore.gitignoreSource [] ./.;
        overlay = self: super: {
        };

      in (haskell.packages.ghc96.extend overlay).callCabal2nix "feigenbaumtree" source {};

    packages.x86_64-linux.image =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "feigenbaumtree";
        src = ./.;
        buildPhase = let
          xDepth = 8;
          yDepth = 3;
          calcDepth = 5;
          size = 512;
          scriptPath = "haskell.magick";
          imagePath = "haskell.png";
        in ''
          feigenbaumtree --x-depth "${toString xDepth}" --y-depth "${toString yDepth}" --calc-depth "${toString calcDepth}" --size "${toString size}" --out-script "\"${scriptPath}\"" --out-image \""${imagePath}\""
          magick-script ${scriptPath}
        '';
        installPhase = ''
          mv haskell.png feigenbaumtree.png
          cp feigenbaumtree.png $out
        '';
        nativeBuildInputs = [
          imagemagick
          self.packages.x86_64-linux.executable
        ];
      };

    devShells.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; };
      haskell.packages.ghc96.shellFor {
        buildInputs = with haskell.packages.ghc96; [
          cabal-install
#          ghcid
          haskellPackages.fourmolu
          haskell-language-server
#          hlint
          implicit-hie
          rnix-lsp
          pkgs.imagemagick
        ];
        packages = haskellPackages: [
          self.packages.x86_64-linux.executable
        ];
        withHoogle = false;
      };

    checks.x86_64-linux.build = self.packages.x86_64-linux.default;

    checks.x86_64-linux.shell = self.devShells.x86_64-linux.default;

    checks.x86_64-linux.warning =
      with import nixpkgs { system = "x86_64-linux"; };
      let override = old: {
        configureFlags = [
          # TODO: Enable -Werror with GHC 9.6.3.
          #"--ghc-option=-Werror"
        ];
      };
      in haskell.lib.overrideCabal self.packages.x86_64-linux.default override;

  };
}
