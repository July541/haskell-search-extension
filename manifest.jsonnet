local manifest = import "./manifest.libsonnet";

local icons() = {
  [size]: "logo.png"
  for size in ["16", "48", "128"]
};

local json = manifest.new(
  name="Haskell Search Extensions",
  version="0.0.1",
  keyword="hs",
  description="no desc"
).addIcons(icons())
 .addBackgroundScripts("./extension/main.js");

json
