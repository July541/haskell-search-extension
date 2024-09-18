import fuzzysort from "fuzzysort";
import { CommandHandler, SearchCache } from "./type";
import { LinkData, linkData } from "../data/link/linkData";
import { Compat } from "../Compat";

export default class LinkHandler extends CommandHandler {
  public static TRIGGER_PREFIX: string = ":url";

  public static isLinkMode(input: string): boolean {
    return this.hasTriggerPrefix(input, this.TRIGGER_PREFIX);
  }

  handleChange(input: string, cache: SearchCache): chrome.omnibox.SuggestResult[] {
    const suggestions = this.giveSuggestions(input);
    this.adjustSuggestions(suggestions, cache);
    return suggestions;
  }

  handleEnter(input: string, cache: SearchCache): string {
    if (input === cache.currentInput) {
      input = cache.defaultContent;
    }
    const query = this.removeExtensionPrefix(input);
    this.parsePageAndRemovePager(query);
    return linkData.find((x) => x.name.toLowerCase() === this.finalQuery.toLowerCase())?.url || "";
  }

  removeExtensionPrefix(input: string): string {
    return this.removeTriggerPrefix(input, LinkHandler.TRIGGER_PREFIX);
  }

  giveSuggestions(input: string): chrome.omnibox.SuggestResult[] {
    const query = this.removeExtensionPrefix(input);
    this.parsePageAndRemovePager(query);
    const startCount = this.curPage * this.PAGE_SIZE;
    const endCount = startCount + this.PAGE_SIZE;
    let suggestLinkData: LinkData[] = fuzzysort
      .go(this.finalQuery, linkData, { key: "name", all: true })
      .map((x) => x.obj);
    this.totalPage = Math.ceil(suggestLinkData.length / this.PAGE_SIZE);
    const suggestions = suggestLinkData.slice(startCount, endCount).map(LinkHandler.linkToSuggestResult);
    return suggestions;
  }

  static linkToSuggestResult(link: LinkData): chrome.omnibox.SuggestResult {
    return {
      content: LinkHandler.TRIGGER_PREFIX + " " + link.name,
      description: Compat.taggedMatch(link.name) + " " + link.description,
    };
  }

  adjustSuggestions(suggestions: chrome.omnibox.SuggestResult[], cache: SearchCache) {
    const head = suggestions.shift();
    if (head) {
      cache.defaultContent = head.content;
      chrome.omnibox.setDefaultSuggestion({
        description: head.description + this.pageMessage(),
      });
    }
  }
}
