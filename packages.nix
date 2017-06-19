{ mkDerivation, base, clock, containers, lens, linear, mtl, random
, stdenv, text
}:
mkDerivation {
  pname = "hack-slash";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base clock containers lens linear mtl random text
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/uncannyworks/hack-slash#readme";
  description = "Entity Component System Game Engine";
  license = stdenv.lib.licenses.unfree;
}
