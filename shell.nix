{ package ? "diet", compiler ? "ghc822" }:

(import ./default.nix {
  inherit package compiler;
}).diet
