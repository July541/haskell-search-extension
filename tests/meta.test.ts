import MetaHandler from "../extension/omnibox/command/meta";
import { testSuggestion0 } from "./util";

describe("meta", () => {
  it("meta trigger", () => {
    const input = ":";
    expect(MetaHandler.isMetaMode(input)).toBe(true);
  });

  it("meta trigger with char", () => {
    const input = ":a";
    expect(MetaHandler.isMetaMode(input)).toBe(true);
  });

  it(":pkg", () => {
    const input = ":pkg";
    testSuggestion0(input, new MetaHandler(), {
      content: ":pkg",
      description: "<match>:pkg</match> - Search Hackage packages.",
    });
  });

  it(":h", () => {
    const input = ":h";
    testSuggestion0(input, new MetaHandler(), {
      content: ":hg",
      description: "<match>:hg</match> - Search on hoogle",
    });
  });

  it(":hg", () => {
    const input = ":hg";
    testSuggestion0(input, new MetaHandler(), {
      content: ":hg",
      description: "<match>:hg</match> - Search on hoogle",
    });
  });

  it(":ext", () => {
    const input = ":ext";
    testSuggestion0(input, new MetaHandler(), {
      content: ":ext",
      description: "<match>:ext</match> - Search Haskell language extensions",
    });
  });

  it(":url", () => {
    const input = ":url";
    testSuggestion0(input, new MetaHandler(), {
      content: ":url",
      description: "<match>:url</match> - Search useful links",
    });
  });

  it(":err", () => {
    const input = ":err";
    testSuggestion0(input, new MetaHandler(), {
      content: ":err",
      description: "<match>:err</match> - Search Haskell errors",
    });
  });
});
