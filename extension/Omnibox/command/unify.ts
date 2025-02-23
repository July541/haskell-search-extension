import fuzzysort from "fuzzysort";
import { Command, CommandHandler, SearchCache } from "./type";
import { HackageData, hackageData } from "../data/hackage/hackageData";
import { Compat } from "../Compat";
import HoogleHandler from "./hoogle";
import PackageHandler from "./package";
import ExtensionHandler from "./extension";
import LinkHandler from "./link";
import ErrorHandler from "./error";
import { extensionData, ExtensionData } from "../data/extension/extensionData";
import { linkData, LinkData } from "../data/link/linkData";
import { errorData, ErrorData } from "../data/error/errorData";

type SubData = HackageData | ExtensionData | LinkData | ErrorData;

enum SearchKey {
  SearchByName,
  SearchByTitle,
  SearchByCode,
  SearchByIntroduced,
}

class UnifyData {
  name: Fuzzysort.Prepared;
  value: SubData;
  searchKey: SearchKey;

  constructor(name: string, data: SubData, searchKey: SearchKey = SearchKey.SearchByName) {
    this.name = fuzzysort.prepare(name);
    this.value = data;
    this.searchKey = searchKey;
  }
}

export default class UnifyHandler extends CommandHandler {
  private searchTargets = ([] as UnifyData[]).concat(
    hackageData.map((x) => new UnifyData(x.name, x)),
    extensionData.map((x) => new UnifyData(x.name, x)),
    linkData.map((x) => new UnifyData(x.name, x)),
    errorData.map((x) => new UnifyData(x.title, x, SearchKey.SearchByTitle)), // We have 3 keys for error search
    errorData.map((x) => new UnifyData(x.code, x, SearchKey.SearchByCode)),
    errorData.map((x) => new UnifyData(x.introduced, x, SearchKey.SearchByIntroduced))
  );

  handleChange(input: string, cache: SearchCache): chrome.omnibox.SuggestResult[] {
    const suggestions = this.giveSuggestions(input);
    this.adjustSuggestions(suggestions, cache);
    return suggestions;
  }

  handleEnter(input: string, cache: SearchCache): string {
    console.log(input);
    console.log(cache);
    if (input === cache.currentInput) {
      input = cache.defaultContent;
    }

    return "";

    const query = this.parsePageAndRemovePager(input);
  }

  private fromHackageData(highlightName: string, data: HackageData): chrome.omnibox.SuggestResult {
    const desp = Compat.escape(data.description);
    const omniboxDescription = "[package] " + (desp.length === 0 ? `${highlightName}` : `${highlightName} - ${desp}`);
    return {
      content: data.name,
      description: omniboxDescription,
    };
  }

  private fromExtensionData(highlightName: string, data: ExtensionData): chrome.omnibox.SuggestResult {
    const extStr = "[extension] ";
    const deprecatedStr = data.deprecated ? "[deprecated] " : "";
    const nameStr = `{-# LANGUAGE ${highlightName} #-} `;
    const sinceStr = data.since ? `Since ${data.since}` : "";
    const includedStr =
      data.included.length === 0 || data.included[0] === "NA" ? "" : `Included in ${data.included.join(",")}`;
    return {
      content: ExtensionHandler.TRIGGER_PREFIX + " " + data.name, // Add <:ext > prefix to make sure that the `handleChange` works.
      description: `${extStr}${deprecatedStr}${nameStr}${sinceStr}${includedStr.length === 0 ? "" : " "}${includedStr}`,
    };
  }

  private fromLinkData(highlightName: string, data: LinkData): chrome.omnibox.SuggestResult {
    return {
      content: LinkHandler.TRIGGER_PREFIX + " " + data.name, // Add <:url > prefix to make sure that the `handleChange` works.
      description: "[link] " + highlightName + " " + data.description,
    };
  }

  private fromErrorData(highlight: string, data: ErrorData, searchKey: SearchKey): chrome.omnibox.SuggestResult {
    switch (searchKey) {
      case SearchKey.SearchByTitle:
        const title = highlight;
        return {
          content: ErrorHandler.TRIGGER_PREFIX + " " + data.code,
          description: `[error] [${data.code}] ${title} (Introduced in ${data.introduced})`,
        };
      case SearchKey.SearchByCode:
        const code = highlight;
        return {
          content: ErrorHandler.TRIGGER_PREFIX + " " + data.code,
          description: `[error] [${code}] ${data.title} (Introduced in ${data.introduced})`,
        };
      case SearchKey.SearchByIntroduced:
        const introduced = highlight;
        return {
          content: ErrorHandler.TRIGGER_PREFIX + " " + data.code,
          description: `[error] [${data.code}] ${data.title} (Introduced in ${introduced})`,
        };
      default:
        throw new Error("Invalid search key");
    }
  }

  unifyToSuggestResult(highlightName: string, data: UnifyData): chrome.omnibox.SuggestResult {
    if (data.value instanceof HackageData) {
      return this.fromHackageData(highlightName, data.value as HackageData);
    } else if (data.value instanceof ExtensionData) {
      return this.fromExtensionData(highlightName, data.value as ExtensionData);
    } else if (data.value instanceof LinkData) {
      return this.fromLinkData(highlightName, data.value as LinkData);
    } else if (data.value instanceof ErrorData) {
      return this.fromErrorData(highlightName, data.value, data.searchKey);
    } else {
      throw new Error("Invalid data type");
    }
  }

  giveSuggestions(input: string): chrome.omnibox.SuggestResult[] {
    this.parsePageAndRemovePager(input);
    const startCount = this.curPage * this.PAGE_SIZE;
    const endCount = startCount + this.PAGE_SIZE;
    const suggestData = fuzzysort.go(this.finalQuery, this.searchTargets, { key: "name", all: true });
    this.totalPage = Math.ceil(suggestData.length / this.PAGE_SIZE);
    const suggestions = suggestData.slice(startCount, endCount).map((x) => {
      const name = x.highlight("<match>", "</match>");
      return this.unifyToSuggestResult(name, x.obj);
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
