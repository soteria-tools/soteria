{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/b12141ef619e0a9c1c84dc8c684040326f27cdcc.tar.gz";
  }) {}
}:

let
  ocamlPackages = pkgs.ocamlPackages.overrideScope (final: prev: {
    ppxlib = prev.ppxlib.overrideAttrs (_: rec {
      version = "0.38.0";
      src = pkgs.fetchFromGitHub {
        owner = "ocaml-ppx";
        repo = "ppxlib";
        tag = version;
        hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
      };
    });
  });
  grace = ocamlPackages.buildDunePackage (finalAttrs: {
    pname = "grace";
    version = "0.3.0";
    src = pkgs.fetchFromGitHub {
      owner = "johnyob";
      repo = "grace";
      tag = "${finalAttrs.version}";
      hash = "sha256-V5K9RGk47K/R+q4wS1FU02kMi1uWSCgdUjKHk7uXuGw=";
    };
    doCheck = false;
    nativeBuildInputs = with ocamlPackages; [
    ];
    propagatedBuildInputs = with ocamlPackages; [
      core_unix
      dedent
      fmt
      iter
      ppx_jane
      uutf
    ];
  });
  htmlit = pkgs.stdenv.mkDerivation rec {
    pname = "ocaml${pkgs.ocaml.version}-htmlit";
    version = "0.2.0";
    src = pkgs.fetchurl {
      url = "https://erratique.ch/software/htmlit/releases/htmlit-${version}.tbz";
      hash = "sha256-i+7gYle8G2y78GeoAnlNY5dpdONLhltuswusCbMmB/c=";
    };
    doCheck = false;
    nativeBuildInputs = with ocamlPackages; [
      ocaml
      findlib
      ocamlbuild
      topkg
    ];
    buildInputs = with ocamlPackages; [ topkg ];
    propagatedBuildInputs = with ocamlPackages; [ astring ];
    strictDeps = true;
    inherit (ocamlPackages.topkg) buildPhase installPhase;
  };
  simple_smt = ocamlPackages.buildDunePackage (finalAttrs: {
    pname = "simple_smt";
    version = "unstable-2025-12-06";
    src = pkgs.fetchFromGitHub {
      owner = "GaloisInc";
      repo = "simple-smt-ocaml";
      rev = "9392032331b2684ec7e421d74634cc676c3f1875";
      hash = "sha256-Nt/gMBljkL84Ic+S42VLhRH8gZ1xPdAmdLC9M5TEzco=";
    };
    doCheck = false;
    propagatedBuildInputs = with ocamlPackages; [
      ppx_deriving
      progress
      sexplib
      zarith
    ];
  });
  printbox = ocamlPackages.buildDunePackage (finalAttrs: {
    pname = "printbox";
    version = "0.12";
    src = pkgs.fetchFromGitHub {
      owner = "c-cube";
      repo = "printbox";
      tag = "v${finalAttrs.version}";
      hash = "sha256-PQbr2sjASoWz0OHAMV6buAJERpnUJxVpLAigIVnADIc=";
    };
    doCheck = false;
  });
  printbox-text = ocamlPackages.buildDunePackage (finalAttrs: {
    pname = "printbox-text";
    inherit (printbox) version src;
    doCheck = false;
    propagatedBuildInputs = with ocamlPackages; [
      printbox
      uucp
      uutf
    ];
  });
  ppx_make = ocamlPackages.buildDunePackage (finalAttrs: {
    pname = "ppx_make";
    version = "0.3.4";
    src = pkgs.fetchFromGitHub {
      owner = "bn-d";
      repo = "ppx_make";
      tag = "v${finalAttrs.version}";
      hash = "sha256-jR+2l5JcB3wT0YsnQCTwptarp4cZwi8GFweQEwSn4oo=";
    };
    doCheck = false;
    propagatedBuildInputs = with ocamlPackages; [
      ppxlib
    ];
  });
  ppx_mixins = ocamlPackages.buildDunePackage (finalAttrs: {
    pname = "ppx_mixins";
    version = "0.2.0";
    src = pkgs.fetchFromGitHub {
      owner = "soteria-tools";
      repo = "ppx_mixins";
      tag = "v${finalAttrs.version}";
      hash = "sha256-Kt+JAZU2JcCSzg/TOikRmGiMsPK7HnIsXSRHk7TPY8c=";
    };
    postPatch = ''
      substituteInPlace dune-project --replace-fail "(lang dune 3.21)" "(lang dune 3.16)"
    '';
    doCheck = false;
    propagatedBuildInputs = with ocamlPackages; [
      ppxlib
    ];
  });
  ppx_subliner = ocamlPackages.buildDunePackage (finalAttrs: {
    pname = "ppx_subliner";
    version = "0.2.2";
    src = pkgs.fetchFromGitHub {
      owner = "bn-d";
      repo = "ppx_subliner";
      tag = "v${finalAttrs.version}";
      hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
    };
    doCheck = false;
    propagatedBuildInputs = with ocamlPackages; [
      cmdliner
      ppxlib
      ppx_make
    ];
  });
  cerberus = pkgs.cerberus.overrideAttrs (old: {
    version = "0-unstable-2025-12-06";
    src = pkgs.fetchFromGitHub {
      owner = "kmemarian";
      repo = "cerberus";
      rev = "1b17799b5877bb180c92d3634b3f30e2fb568264";
      hash = "sha256-Zj1X66r9eu2j1Y3bfTPxIX9Qa3w76VFSdSMmZOh3kNg=";
    };
    doCheck = false;
    doInstallCheck = false;
  });
  soteria = ocamlPackages.buildDunePackage (finalAttrs: {
    pname = "soteria";
    version = "unstable-2025-12-26";
    src = ./.;
    doCheck = false;
    nativeBuildInputs = with ocamlPackages; [
      pkgs.git
    ];
    propagatedBuildInputs = with ocamlPackages; [
      grace
      hc
      htmlit
      iter
      patricia-tree
      ppxlib
      ppx_blob
      ppx_deriving
      ppx_deriving_yojson
      ppx_mixins
      ppx_subliner
      printbox-text
      simple_smt
      tsort
      unionFind
      yojson
      zarith
    ];
  });
  soteria-c = ocamlPackages.buildDunePackage (finalAttrs: {
    pname = "soteria-c";
    version = "unstable-2025-12-26";
    src = ./.;
    dontDetectOcamlConflicts = true;
    nativeBuildInputs = with ocamlPackages; [
      pkgs.git
    ];
    doCheck = false;
    propagatedBuildInputs = with ocamlPackages; [
      cerberus
      dune-site
      eio_main
      lem
      linol-eio
      menhirLib
      pprint
      sha
      soteria
    ];
  });
in

pkgs.mkShell {
  dontDetectOcamlConflicts = true;
  nativeBuildInputs = with ocamlPackages; [
    soteria-c
  ];
}
