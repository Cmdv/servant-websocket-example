{ mkDerivation, aeson, base, bytestring, conduit, containers
, http-types, mtl, servant, servant-server, servant-websockets
, stdenv, text, wai, wai-websockets, warp, websockets
}:
mkDerivation {
  pname = "myappT";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring conduit containers http-types mtl servant
    servant-server servant-websockets text wai wai-websockets warp
    websockets
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
