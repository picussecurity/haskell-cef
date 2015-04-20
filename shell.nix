with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, bytestring, directory, doctest, filepath
             , stdenv, text
             }:
             mkDerivation {
               pname = "cef";
               version = "0.1.0.0";
               src = ./.;
               buildDepends = [ base bytestring text ];
               testDepends = [ base directory doctest filepath ];
               homepage = "http://github.com/picussecurity/haskell-cef.git";
               description = "CEF log format";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
