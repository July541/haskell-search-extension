import UnifyHandler from "../extension/omnibox/command/unify";
import { SearchCache } from "../extension/omnibox/command/type";

describe("package", () => {
  it("Basic package search", () => {
    const input = "arr";
    const handler = new UnifyHandler();
    const suggest = handler.handleChange(input, new SearchCache());
    expect(suggest[0]).toEqual({
      content: "https://hoogle.haskell.org/?hoogle=arr",
      description: "Search arr on [hoogle.haskell.org]",
    });
  });
});
