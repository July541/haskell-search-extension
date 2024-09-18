import LinkHandler from "../extension/omnibox/command/link";
import { testSuggestion0 } from "./util";

describe("link", () => {
  it(":url", () => {
    const input = ":url";
    expect(LinkHandler.isLinkMode(input)).toBe(true);
  });

  it(":url trigger", () => {
    const input = ":url Haskell";
    expect(LinkHandler.isLinkMode(input)).toBe(true);
  });

  it("default behavior", () => {
    testSuggestion0(":url ", new LinkHandler(), {
      content: ":url Haskell",
      description: "<match>Haskell</match> Haskell language official page",
    });
  });

  it("next page", () => {
    testSuggestion0(":url -", new LinkHandler(), {
      content: ":url Unfolder",
      description: "<match>Unfolder</match> The Haskell Unfolder Episode",
    });
  });
});
