local manifest = import "./manifest.libsonnet";

local json = manifest.new(
  name="Haskell Search Extensions",
  version="0.0.1",
  keyword="hs",
  description="no desc"
).addBackgroundScripts("./extension/main.js");

json
