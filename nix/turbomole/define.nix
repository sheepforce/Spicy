{ stdenv, lib, requireFile, autoPatchelfHook, makeWrapper, turbomole } :

stdenv.mkDerivation rec {
  pname = "turbomole-define";
  version = "7.5.1.post1";

  src = requireFile {
    name = "define";
    sha256 = "042db7db094dfb36bd258407ab1c31734c41d13290f2f1c1ee6ad4f09924e7f0";
    message = ''
      This is a custom development version, that replaces the original define script.
      It is not publically available and should only be used for development purposes.
      It writes the new, simple input style by default.
    '';
  };

  nativeBuildInputs = [ makeWrapper autoPatchelfHook ];

  propagatedBuildInputs = [ turbomole ];

  dontUnpack = true;
  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp ${src} $out/bin/define
    chmod +x $out/bin/define

    runHook postInstall
  '';

  postInstall = ''
    wrapProgram $out/bin/define \
      --prefix PATH : "${turbomole}/bin" \
      --set TURBODIR "${turbomole}/share/turbomole"
  '';
}
