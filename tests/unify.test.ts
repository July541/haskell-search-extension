import UnifyHandler from "../extension/omnibox/command/unify";
import { testSuggestion0 } from "./util";

describe("package", () => {
  it("Basic package search", () => {
    testSuggestion0("array", new UnifyHandler(), {
      content: "array",
      description: "[package] <match>array</match> - Mutable and immutable arrays",
    });
  });

  it("Package next page", () => {
    testSuggestion0("arr -", new UnifyHandler(), {
      content: "ContArrow",
      description: "[package] Cont<match>Arr</match>ow - Control.Arrow.Transformer.Cont",
    });
  });

  it("Search extension", () => {
    testSuggestion0("DataKinds", new UnifyHandler(), {
      content: ":ext DataKinds",
      description: "[extension] {-# LANGUAGE <match>DataKinds</match> #-} Since 7.4.1 Included in GHC2024",
    });
  });

  it("Search link", () => {
    testSuggestion0("Hackage", new UnifyHandler(), {
      content: ":url Hackage",
      description: "[link] <match>Hackage</match> The Haskell Package Repository",
    });
  });

  it("Search error by title", () => {
    testSuggestion0("Illegal tuple section", new UnifyHandler(), {
      content: ":err GHC-59155",
      description: "[error] [GHC-59155] <match>Illegal tuple section</match> (Introduced in 9.6.1)",
    });
  });

  it("Search error by code", () => {
    testSuggestion0("GHC-00482", new UnifyHandler(), {
      content: ":err GHC-00482",
      description: "[error] [<match>GHC-00482</match>] Lambda syntax in pattern (Introduced in 9.6.1)",
    });
  });

  it("Search error by introduced", () => {
    testSuggestion0("9.8.1", new UnifyHandler(), {
      content: ":err GHC-93557",
      description: "[error] [GHC-93557] Illegal typeclass instance (Introduced in <match>9.8.1</match>)",
    });
  });
});
