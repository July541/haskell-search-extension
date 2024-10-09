import UnifyHandler from "../extension/omnibox/command/unify";
import { SearchCache } from "../extension/omnibox/command/type";
import { testSuggestion0 } from "./util";

describe("package", () => {
  it("Basic package search", () => {
    testSuggestion0("arr", new UnifyHandler(), {
      content: "array",
      description: "[package] array - Mutable and immutable arrays",
    });
  });
  it("next page", () => {
    testSuggestion0("arr -", new UnifyHandler(), {
      content: "bit-array",
      description: "[package] bit-array - A bit array (aka bitset, bitmap, bit vector) API for numeric types",
    });
  });
});
