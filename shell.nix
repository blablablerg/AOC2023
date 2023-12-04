with import <nixpkgs> {};

stdenv.mkDerivation {
	name = "AOC2023";
	buildInputs = [
	  (pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs;
      [ 
        haskell-language-server
        hoogle
        pretty-simple
      ]))
	];

}
