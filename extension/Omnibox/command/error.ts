import fuzzysort from "fuzzysort";
import { CommandHandler, SearchCache } from "./type";
import { ErrorData, errorData } from "../data/error/errorData";

export default class ErrorHandler extends CommandHandler {
  public static TRIGGER_PREFIX: string = ":err";

  public static isErrorMode(input: string): boolean {
    return this.hasTriggerPrefix(input, this.TRIGGER_PREFIX);
  }

  handleChange(input: string, cache: SearchCache): chrome.omnibox.SuggestResult[] {
    const suggestions = this.giveSuggestions(input);
    this.adjustSuggestions(suggestions, cache);
    return suggestions;
  }

  handleEnter(input: string, cache: SearchCache): string {
    const query = this.removeErrorPrefix(input);
    this.parsePageAndRemovePager(query);

    const err = errorData.find((x) => x.code === this.finalQuery);
    return `https://errors.haskell.org/${err ? err.route : ""}`;
  }

  removeErrorPrefix(input: string): string {
    return this.removeTriggerPrefix(input, ErrorHandler.TRIGGER_PREFIX);
  }

  giveSuggestions(input: string): chrome.omnibox.SuggestResult[] {
    const query = this.removeErrorPrefix(input);
    this.parsePageAndRemovePager(query);
    const startCount = this.curPage * this.PAGE_SIZE;
    const endCount = startCount + this.PAGE_SIZE;
    const suggestErrData = fuzzysort.go(this.finalQuery, errorData, {
      keys: ["title", "code", "introduced"],
      all: true,
    });
    this.totalPage = Math.ceil(suggestErrData.length / this.PAGE_SIZE);
    const suggestions = suggestErrData
      .slice(startCount, endCount)
      .map((x) => x.obj)
      .map(this.errorToSuggestResult);
    return suggestions;
  }

  private errorToSuggestResult(err: ErrorData): chrome.omnibox.SuggestResult {
    return {
      content: ErrorHandler.TRIGGER_PREFIX + " " + err.code,
      description: `[${err.code}] ${err.title} (Introduced in ${err.introduced})`,
    };
  }

  adjustSuggestions(suggestions: chrome.omnibox.SuggestResult[], cache: SearchCache) {
    const head = suggestions.shift();
    if (head) {
      cache.defaultContent = head.content;
      chrome.omnibox.setDefaultSuggestion({ description: head.description + this.pageMessage() });
    }
  }
}
