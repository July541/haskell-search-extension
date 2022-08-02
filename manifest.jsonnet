local manifest = import "./manifest.libsonnet";

local icons() = {
  [size]: "logo.png"
  for size in ["16", "48", "128"]
};

local json = manifest.new(
  name="Haskell Search Extension",
  version="0.0.1",
  keyword="hs",
  description="The search extension for Haskell language's packages and functions.",
  service_worker="./extension/main.js"
).addIcons(icons());

json
