{s}:
let ghcOptions = ["-Wall" "-fdefer-typed-holes"];
    replOptions = ''--repl-options "${builtins.concatStringsSep " " ghcOptions}"'';
in {
  devParconcScript = s "dev:parconc" "ghcid --command 'cabal new-repl lib:parconc ${replOptions}' --allow-eval --warnings";
  devExperimentsScript = s "dev:experiments" "ghcid --command 'cabal new-repl lib:experiments ${replOptions}' --allow-eval --warnings";
  replParconcScript = s "repl" "cabal new-repl lib:parconc";
  replExperimentsScript = s "repl:experiments" "cabal new-repl lib:experiments";
  hoogleScript = s "hgl" "hoogle serve -p 9001";
}
