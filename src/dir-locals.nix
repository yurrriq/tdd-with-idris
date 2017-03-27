with (import <nixpkgs> {});

# let
#   env = callPackage ../shell.nix {};
# in
#
# nixBufferBuilders.withPackages [ env ]

# HACK: use hand-built idris for now
nixBufferBuilders.withPackages [ gcc gmp libffi ]
