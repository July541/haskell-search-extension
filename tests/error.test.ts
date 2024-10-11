import ErrorHandler from "../extension/omnibox/command/error";
import { testSuggestion0 } from "./util";

describe("error", () => {
  it(":err", () => {
    const input = ":err";
    expect(ErrorHandler.isErrorMode(input)).toBe(true);
  });

  it(":err trigger", () => {
    const input = ":err ambiguous";
    expect(ErrorHandler.isErrorMode(input)).toBe(true);
  });

  it("default behavior", () => {
    testSuggestion0(":err ambiguous", new ErrorHandler(), {
      content: ":err GHC-02256",
      description: "[GHC-02256] Ambiguous record update (Introduced in 9.6.1)",
    });
  });

  it("suggestion number", () => {
    const input = ":err ghc";
    const handler = new ErrorHandler();
    const suggestions = handler.giveSuggestions(input);
    expect(suggestions.length).toBe(10);
  });
});
