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
        source = ./haskell;
        overlay = self: super: {
        };

      in (haskell.packages.ghc96.extend overlay).callCabal2nix "feigenbaumtree" source {};

    packages.x86_64-linux.image-1 =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "feigenbaumtree-image-1";
        src = ./haskell;
        buildPhase = let
          xDepth = 12;
          yDepth = 8;
          calcDepth = 5;
          size = 4096;
          scriptPath = "haskell.magick";
          imagePath = "haskell.png";
        in ''
          feigenbaumtree --x-depth "${toString xDepth}" --y-depth "${toString yDepth}" --calc-depth "${toString calcDepth}" --size "${toString size}" --out-script "\"${scriptPath}\"" --out-image \""${imagePath}\""
          magick-script ${scriptPath}
        '';
        installPhase = ''
          mkdir $out
          mv haskell.png $out/feigenbaumtree.png
        '';
        nativeBuildInputs = [
          imagemagick
          self.packages.x86_64-linux.executable
        ];
      };

    packages.x86_64-linux.image-2 =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "feigenbaumtree-image-2";
        src = ./magick;
        buildPhase = ''
          cp ${self.packages.x86_64-linux.image-1.outPath}/feigenbaumtree.png haskell.png
          magick-script improve-color.magick
          magick-script adjust-size.magick
          magick-script background.magick
          magick-script compose.magick
        '';
        installPhase = ''
          mkdir $out
          mv composed.png $out/feigenbaumtree.png
        '';
        nativeBuildInputs = [
          imagemagick
          fira
          self.packages.x86_64-linux.image-1
        ];
      };

    packages.x86_64-linux.font =
      with import nixpkgs { system = "x86_64-linux"; };
      pkgs.fira;

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
          pkgs.fira
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
