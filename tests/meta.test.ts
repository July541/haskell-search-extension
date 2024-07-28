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

  it(":package", () => {
    const input = ":package";
    testSuggestion0(input, new MetaHandler(), {
      content: ":package",
      description: "<match>:package</match> - Search Hackage packages.",
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

  it(":hoogle", () => {
    const input = ":hoogle";
    testSuggestion0(input, new MetaHandler(), {
      content: ":hoogle",
      description: "<match>:hoogle</match> - Search on hoogle",
    });
  });

  it(":extension", () => {
    const input = ":extension";
    testSuggestion0(input, new MetaHandler(), {
      content: ":extension",
      description: "<match>:extension</match> - Search Haskell language extensions",
    });
  });

  it(":lan", () => {
    const input = ":lan";
    testSuggestion0(input, new MetaHandler(), {
      content: ":lan",
      description: "<match>:lan</match> - Search Haskell language extensions",
    });
  });

  it(":lang", () => {
    const input = ":lang";
    testSuggestion0(input, new MetaHandler(), {
      content: ":lang",
      description: "<match>:lang</match> - Search Haskell language extensions",
    });
  });

  it(":language", () => {
    const input = ":language";
    testSuggestion0(input, new MetaHandler(), {
      content: ":language",
      description: "<match>:language</match> - Search Haskell language extensions",
    });
  });
});
