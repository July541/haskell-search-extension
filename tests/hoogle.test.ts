import HoogleHandler from "../extension/omnibox/command/hoogle";
import { testSuggestion0 } from "./util";

describe("hoogle", () => {
  it(":hg", () => {
    const input = ":hg";
    expect(HoogleHandler.isHoogleMode(input)).toBe(true);
  });

  it("description for :hg", () => {
    testSuggestion0(":hg", new HoogleHandler(), {
      content: "https://hoogle.haskell.org",
      description: "Continue typing or press entering to hoogle.com",
    });
  });

  it(":hg trigger", () => {
    const input = ":hg arrow";
    expect(HoogleHandler.isHoogleMode(input)).toBe(true);
  });

  it("hoogle url with :hg", () => {
    testSuggestion0(":hg arrow", new HoogleHandler(), {
      content: "https://hoogle.haskell.org/?hoogle=arrow",
      description: "Search <match>arrow</match> on [hoogle.haskell.org]",
    });
  });
});
