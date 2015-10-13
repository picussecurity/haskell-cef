with (import <nixpkgs> {}).pkgs;
let pkg = haskell.packages.ghc7102.callPackage
            ({ mkDerivation, base, bytestring, directory, doctest, filepath
             , stdenv, text, cabal-install
             }:
             mkDerivation {
               pname = "cef";
               version = "0.1.2";
               src = ./.;
               buildDepends = [ base bytestring text cabal-install ];
               testDepends = [ base directory doctest filepath ];
               homepage = "http://github.com/picussecurity/haskell-cef.git";
               description = "CEF log format";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
