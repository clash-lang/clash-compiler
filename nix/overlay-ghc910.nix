{ pkgs }:
final: prev:
let
  inherit (pkgs.haskell.lib) dontCheck doJailbreak markUnbroken;
in
{
  # Use an older version than the default in nixpkgs. Since rewrite-inspector
  # is basically abandonware it catches fire with brick 1.0+.
  brick = doJailbreak prev.brick_0_70_1;

  vty = doJailbreak (prev.callHackage "vty" "5.39" { });

  hint = doJailbreak prev.hint;

  th-desugar = prev.th-desugar_1_17;

  singletons-th = prev.singletons-th_3_4;

  # Marked as broken in nixpkgs, since it specifies much older dependencies
  # than the defaults in nixpkgs.
  rewrite-inspector = doJailbreak (markUnbroken prev.rewrite-inspector);

  derive-storable-plugin = doJailbreak prev.derive-storable-plugin;

  # Marken as broken, but compiles anyway.
  hedgehog-fakedata = doJailbreak (markUnbroken prev.hedgehog-fakedata);

  # Fails on GHC 9.10 with:
  #   library/Text/Regex/PCRE/Heavy.hs:123: failure in expression `head $ scan [re|\s*entry (\d+) (\w+)\s*&?|] (" entry 1 hello  &entry 2 hi" :: String)'                                                           
  # expected: (" entry 1 hello  &",["1","hello"])                                                                                                                                                                 
  #  but got: <interactive>:55:1: warning: [GHC-63394] [-Wx-partial]                                                                                                                                              
  #           ^                                                                                                                                                                                  
  #               In the use of ‘head’                                                                                                                                                                 
  #               (imported from Prelude.Compat, but defined in GHC.Internal.List):
  #               "This is a partial function, it throws an error on empty lists. Use pattern matching, 'Data.List.uncons' or 'Data.Maybe.listToMaybe' instead. Consider refactoring to use "Data.List.NonEmpty"."
  # (" entry 1 hello  &",["1","hello"])
                                                                                                                                                                                                              
  pcre-heavy = dontCheck prev.pcre-heavy;

  # Relies on older versions of text.
  string-random = doJailbreak prev.string-random;

  singletons-base = prev.singletons-base_3_4;

  microstache = prev.microstache_1_0_3;

  # nixplgs doesn't include revision 1, changing dependency on template-haskell.
  string-interpolate = pkgs.haskell.lib.compose.overrideCabal (drv: {
    revision = "1";
    editedCabalFile = "sha256-oh0tR+LDFcVzQnm4kSrmhAU+P7wdai536d72Cvhzipg=";
  }) (prev.callHackageDirect {
    pkg = "string-interpolate";
    ver = "0.3.4.0";
    sha256 = "sha256-KA8P6cc6N7pZ9/ay3edcEGx4vpKtp+EY7tn8U1NrbG8=";
  } { });
}
