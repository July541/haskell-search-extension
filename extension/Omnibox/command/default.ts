import fuzzysort from "fuzzysort";
import { errorData, ErrorData } from "../data/error/errorData";
import { extensionData, ExtensionData } from "../data/extension/extensionData";
import { hackageData, HackageData } from "../data/hackage/hackageData";
import { CommandHandler, SearchCache } from "./type";
import HoogleHandler from "./hoogle";
import { Compat } from "../Compat";
import ExtensionHandler from "./extension";
import ErrorHandler from "./error";

class ExtensionDataFromName {
  data: ExtensionData;

  constructor(data: ExtensionData) {
    this.data = data;
  }
}

class ExtensionDataFromVersion {
  data: ExtensionData;

  constructor(data: ExtensionData) {
    this.data = data;
  }
}

type TopicData = HackageData | ExtensionDataFromName | ExtensionDataFromVersion | ErrorData;

enum SearchExtensionFrom {
  FromName,
  FromVersion,
}

class SearchData {
  name: Fuzzysort.Prepared;
  data: TopicData;

  constructor(name: string, data: TopicData) {
    this.name = fuzzysort.prepare(name);
    this.data = data;
  }
}

export default class DefaultHandler extends CommandHandler {
  private searchTargets = ([] as SearchData[]).concat(
    // Search package by package name
    hackageData.map((x) => new SearchData(x.name, x)),
    // Search extension by extension name
    extensionData.map((x) => new SearchData(x.name, new ExtensionDataFromName(x))),
    // Search extension by included version
    extensionData.map((x) => x.included.map((y) => new SearchData(y, new ExtensionDataFromVersion(x)))).flat(),
    // Search error by error code
    errorData.map((x) => new SearchData(x.code, x))
  );

  handleChange(input: string, cache: SearchCache): chrome.omnibox.SuggestResult[] {
    const suggestions = this.giveSuggestions(input);
    this.adjustSuggestions(suggestions, cache);
    return suggestions;
  }

  handleEnter(input: string, cache: SearchCache): string {
    console.log("---", input);
    if (input === cache.currentInput) {
      // If the input is the same as the this.currentInput,
      // that means the user wants to use the first search result.
      // So we need to use the default content as the search target(like package name)
      input = cache.defaultContent;
    }
    this.parsePageAndRemovePager(input);
    const url = `https://hackage.haskell.org/package/${this.finalQuery}`;
    return url;
  }

  private fromHackageData(highlightName: string, data: HackageData): chrome.omnibox.SuggestResult {
    const desp = Compat.escape(data.description);
    const omniboxDescription = "[package] " + (desp.length === 0 ? `${highlightName}` : `${highlightName} - ${desp}`);
    return {
      content: data.name,
      description: omniboxDescription,
    };
  }

  private fromExtensionData(
    highlight: string,
    data: ExtensionData,
    from: SearchExtensionFrom
  ): chrome.omnibox.SuggestResult {
    const extStr = "[extension] ";
    const deprecatedStr = data.deprecated ? "[deprecated] " : "";
    let nameStr, sinceStr;
    if (from == SearchExtensionFrom.FromName) {
      nameStr = `{-# LANGUAGE ${highlight} #-} `;
      sinceStr = data.since ? `Since ${data.since}` : "";
    } else {
      nameStr = `{-# LANGUAGE ${data.name} #-} `;
      sinceStr = `Since ${highlight}`;
    }
    const includedStr =
      data.included.length === 0 || data.included[0] === "NA" ? "" : `Included in ${data.included.join(",")}`;
    return {
      content: ExtensionHandler.TRIGGER_PREFIX + " " + data.name, // Add <:ext > prefix to make sure that the `handleChange` works.
      description: `${extStr}${deprecatedStr}${nameStr}${sinceStr}${includedStr.length === 0 ? "" : " "}${includedStr}`,
    };
  }

  private fromErrorData(highlight: string, data: ErrorData): chrome.omnibox.SuggestResult {
    const code = highlight;
    return {
      content: ErrorHandler.TRIGGER_PREFIX + " " + data.code,
      description: `[error] [${code}] ${data.title} (Introduced in ${data.introduced})`,
    };
  }

  toSuggestionResult(highlightName: string, search: SearchData): chrome.omnibox.SuggestResult {
    const data = search.data;
    if (data instanceof HackageData) {
      return this.fromHackageData(highlightName, data);
    } else if (data instanceof ErrorData) {
      return this.fromErrorData(highlightName, data);
    } else if (data instanceof ExtensionDataFromName) {
      return this.fromExtensionData(highlightName, data.data, SearchExtensionFrom.FromName);
    } else if (data instanceof ExtensionDataFromVersion) {
      return this.fromExtensionData(highlightName, data.data, SearchExtensionFrom.FromVersion);
    } else {
      throw new Error("Unknown data type");
    }
  }

  giveSuggestions(input: string): chrome.omnibox.SuggestResult[] {
    this.parsePageAndRemovePager(input);
    const startCount = this.curPage * this.PAGE_SIZE;
    const endCount = startCount + this.PAGE_SIZE;

    const candidates = fuzzysort.go(this.finalQuery, this.searchTargets, { key: "name", all: true });
    this.totalPage = Math.ceil(candidates.length / this.PAGE_SIZE);
    const suggestions = candidates.slice(startCount, endCount).map((x) => {
      const name = x.highlight("<match>", "</match>");
      return this.toSuggestionResult(name, x.obj);
    });

    return suggestions;
  }

  adjustSuggestions(suggestions: chrome.omnibox.SuggestResult[], cache: SearchCache) {
    const head = suggestions.shift();
    if (head) {
      cache.defaultContent = head.content;
      chrome.omnibox.setDefaultSuggestion({ description: head.description + this.pageMessage() });
    } else {
      const hoogle = HoogleHandler.buildHoogleSuggestResult(this.finalQuery);
      chrome.omnibox.setDefaultSuggestion({ description: hoogle.description });
      cache.defaultContent = cache.currentInput;
      suggestions = [hoogle];
    }
  }
}
