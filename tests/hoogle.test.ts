import HoogleHandler from "../extension/omnibox/command/hoogle";
import { SearchCache } from "../extension/omnibox/command/type";

describe("hoogle", () => {
  it(":hg trigger", () => {
    const input = ":hg arrow";
    expect(HoogleHandler.isHoogleMode(input)).toBe(true);
  });

  it(":hoogle trigger", () => {
    const input = ":hoogle arrow";
    expect(HoogleHandler.isHoogleMode(input)).toBe(true);
  });

  it("hoogle url with :hg", () => {
    const handler = new HoogleHandler();
    const query = handler.removeHooglePrefix(":hg arrow");
    const result = HoogleHandler.buildHoogleSuggestResult(query);
    expect(result).toEqual({
      content: "https://hoogle.haskell.org/?hoogle=arrow",
      description: "Search arrow on [hoogle.haskell.org]",
    });
  });
});
