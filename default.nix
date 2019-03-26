{ stdenv, fetchFromGitHub, ocaml, findlib, ppx_tools, stdint }:

stdenv.mkDerivation rec {
  pname = "ocaml${ocaml.version}-pretty-printers-parsers";
  version = "2.7.5";

  src = fetchFromGitHub {
    owner = "rixed";
    repo = "ocaml-pretty-printers-parsers";
    rev = "v${version}";
    sha256 = "06kk2cvnfpqhk3l7d1gl43jaarpgizsilag56cm1jq6yyp8zb2yg";
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
