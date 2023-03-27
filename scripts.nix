{s}:
let ghcOptions = ["-Wall" "-fdefer-typed-holes"];
    replOptions = ''--repl-options "${builtins.concatStringsSep " " ghcOptions}"'';
in {
  devScript = s "dev" "ghcid --command 'cabal new-repl lib:parconc ${replOptions}' --allow-eval --warnings";
  replScript = s "repl" "cabal new-repl lib:parconc";
  hoogleScript = s "hgl" "hoogle serve -p 9001";
}
