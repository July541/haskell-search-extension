import DefaultHandler from "../extension/omnibox/command/default";
import { testSuggestion0 } from "./util";

describe("package", () => {
  it("Basic package search", () => {
    testSuggestion0("array", new DefaultHandler(), {
      content: "array",
      description: "[package] <match>array</match> - Mutable and immutable arrays",
    });
  });

  it("Package next page", () => {
    testSuggestion0("arr -", new DefaultHandler(), {
      content: "bit-array",
      description:
        "[package] bit-<match>arr</match>ay - A bit array (aka bitset, bitmap, bit vector) API for numeric types",
    });
  });

  it("Search extension", () => {
    testSuggestion0("DataKinds", new DefaultHandler(), {
      content: ":ext DataKinds",
      description: "[extension] {-# LANGUAGE <match>DataKinds</match> #-} Since 7.4.1 Included in GHC2024",
    });
  });

  it("Search link", () => {
    testSuggestion0("Hackage", new DefaultHandler(), {
      content: "hackager",
      description: "[package] <match>hackage</match>r - Hackage testing tool",
    });
  });

  it("Search error by code", () => {
    testSuggestion0("GHC-00482", new DefaultHandler(), {
      content: ":err GHC-00482",
      description: "[error] [<match>GHC-00482</match>] Lambda syntax in pattern (Introduced in 9.6.1)",
    });
  });
});
