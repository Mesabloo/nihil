{
  pkgs ? import <nixpkgs> {}
, ghc ? pkgs.ghc
}:

pkgs.haskell.lib.buildStackProject {
  inherit ghc;

  name = "nihil-shell";
}
