{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz";
  }) {}
}:

let
  grace = pkgs.ocamlPackages.buildDunePackage (finalAttrs: {
    pname = "grace";
    version = "0.2.0";
    src = pkgs.fetchFromGitHub {
      owner = "johnyob";
      repo = "grace";
      tag = "${finalAttrs.version}";
      hash = "sha256-jubzimeKs29Y6Di2/kpKEOnNAEzMzVpC5HMLjog4Tlg=";
    };
    doCheck = false;
    nativeBuildInputs = with pkgs.ocamlPackages; [
    ];
    propagatedBuildInputs = with pkgs.ocamlPackages; [
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
    nativeBuildInputs = with pkgs.ocamlPackages; [
      ocaml
      findlib
      ocamlbuild
      topkg
    ];
    buildInputs = with pkgs.ocamlPackages;[ topkg ];
    propagatedBuildInputs = with pkgs.ocamlPackages; [ astring ];
    strictDeps = true;
    inherit (pkgs.ocamlPackages.topkg) buildPhase installPhase;
  };
  simple_smt = pkgs.ocamlPackages.buildDunePackage (finalAttrs: {
    pname = "simple_smt";
    version = "unstable-2025-12-06";
    src = pkgs.fetchFromGitHub {
      owner = "GaloisInc";
      repo = "simple-smt-ocaml";
      rev = "9392032331b2684ec7e421d74634cc676c3f1875";
      hash = "sha256-Nt/gMBljkL84Ic+S42VLhRH8gZ1xPdAmdLC9M5TEzco=";
    };
    doCheck = false;
    propagatedBuildInputs = with pkgs.ocamlPackages; [
      ppx_deriving
      progress
      sexplib
      zarith
    ];
  });
  printbox = pkgs.ocamlPackages.buildDunePackage (finalAttrs: {
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
  printbox-text = pkgs.ocamlPackages.buildDunePackage (finalAttrs: {
    pname = "printbox-text";
    inherit (printbox) version src;
    doCheck = false;
    propagatedBuildInputs = with pkgs.ocamlPackages; [
      printbox
      uucp
      uutf
    ];
  });
  ppx_make = pkgs.ocamlPackages.buildDunePackage (finalAttrs: {
    pname = "ppx_make";
    version = "0.3.4";
    src = pkgs.fetchFromGitHub {
      owner = "bn-d";
      repo = "ppx_make";
      tag = "v${finalAttrs.version}";
      hash = "sha256-jR+2l5JcB3wT0YsnQCTwptarp4cZwi8GFweQEwSn4oo=";
    };
    doCheck = false;
    propagatedBuildInputs = with pkgs.ocamlPackages; [
      ppxlib
    ];
  });
  ppx_subliner = pkgs.ocamlPackages.buildDunePackage (finalAttrs: {
    pname = "ppx_subliner";
    version = "0.2.1";
    src = pkgs.fetchFromGitHub {
      owner = "bn-d";
      repo = "ppx_subliner";
      tag = "v${finalAttrs.version}";
      hash = "sha256-uqOYn4hdkbfiP2hEg/WA+CxwRtqaV2G5rE8P9gh/4ts=";
    };
    doCheck = false;
    propagatedBuildInputs = with pkgs.ocamlPackages; [
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
  });
  soteria = pkgs.ocamlPackages.buildDunePackage (finalAttrs: {
    pname = "soteria";
    version = "unstable-2025-12-26";
    src = ./.;
    doCheck = false;
    nativeBuildInputs = with pkgs.ocamlPackages; [
      pkgs.git
    ];
    propagatedBuildInputs = with pkgs.ocamlPackages; [
      grace
      hc
      htmlit
      iter
      ppxlib
      ppx_blob
      ppx_deriving
      ppx_deriving_yojson
      ppx_subliner
      printbox-text
      simple_smt
      tsort
      unionFind
      yojson
      zarith
    ];
  });
  soteria-c = pkgs.ocamlPackages.buildDunePackage (finalAttrs: {
    pname = "soteria-c";
    version = "unstable-2025-12-26";
    src = ./.;
    nativeBuildInputs = with pkgs.ocamlPackages; [
      pkgs.git
    ];
    doCheck = false;
    propagatedBuildInputs = with pkgs.ocamlPackages; [
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
  nativeBuildInputs = with pkgs.ocamlPackages; [
    soteria-c
  ];
}
