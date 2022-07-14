local manifest = import "./manifest.libsonnet";
local json = manifest.new(
  name="Haskell Search Extension",
  version="0.0.1",
  keyword="hs",
  description="no desc"
);

json
