{
  spicyStatic = (import ./default.nix {
    static = true;
    wrap = false;
  }).spicy.components.exes;

  spicyWrapped = (import ./default.nix {
    static = false;
    wrap = true;
  }).spicy.components.exes;

  spicyProfiling = (import ./default.nix {
    static = false;
    wrap = false;
    profiling = true; }
  ).spicy.components.exes;
}
