{ mkDerivation, base, bytestring, directory, doctest, filepath
, stdenv, text, time
}:
mkDerivation {
  pname = "cef";
  version = "0.1.5";
  src = ./.;
  libraryHaskellDepends = [ base bytestring text time ];
  testHaskellDepends = [ base directory doctest filepath ];
  homepage = "http://github.com/picussecurity/haskell-cef.git";
  description = "CEF log format";
  license = stdenv.lib.licenses.bsd3;
}
