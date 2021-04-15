final: previous: {
  haskellPackages = previous.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (self: super: {
      fld-logger = final.haskell.lib.buildStrictly
        (self.callCabal2nixWithOptions "servant-sns-webhook" (final.gitignoreSource ../servant-sns-webhook) "--no-hpack" { });
    });
  });
}
