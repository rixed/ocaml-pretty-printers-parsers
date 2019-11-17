{ stdenv, fetchFromGitHub, ocaml, findlib, ppx_tools, stdint }:

stdenv.mkDerivation rec {
  pname = "ocaml${ocaml.version}-pretty-printers-parsers";
  version = "2.8.2";

  src = fetchFromGitHub {
    owner = "rixed";
    repo = "ocaml-pretty-printers-parsers";
    rev = "v2.8.2";
    sha256 = "0h35cwvg1kprj7nayy722was5aj1hb0n4s323p8mqr70qg99ci9n";
  };

  buildInputs = [ ocaml findlib ppx_tools stdint ];
  installPhase = ''
    make install bin_dir=$out/bin
  '';

  createFindlibDestdir = true;

  meta = with stdenv.lib; {
    homepage = https://github.com/rixed/ocaml-pretty-printers-parsers;
    description = "Pretty-Printers/Parsers";
    platforms = ocaml.meta.platforms or [];
    maintainers = [ maintainers.rixed ];
  };
}
