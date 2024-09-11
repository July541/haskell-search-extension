import fuzzysort from "fuzzysort";
import { ExtensionData, ExtensionSetVersion, extensionData } from "../data/extension/extensionData";
import { CommandHandler, SearchCache } from "./type";
import HoogleHandler from "./hoogle";

export default class ExtensionHandler extends CommandHandler {
  public static TRIGGER_PREFIX: string = ":ext";
  private static EXT_MAP: Map<string, string> = new Map(extensionData.map((x) => [x.name, x.url]));

  public static isExtensionMode(input: string): boolean {
    return this.hasTriggerPrefix(input, this.TRIGGER_PREFIX);
  }

  private static extensionToSuggestResult(extension: ExtensionData): chrome.omnibox.SuggestResult {
    return {
      content: ExtensionHandler.TRIGGER_PREFIX + " " + extension.name, // Add <:ext > prefix to make sure that the `handleChange` works.
      description: `${extension.deprecated ? "[deprecated] " : ""}{-# LANGUAGE ${extension.name} #-} Since ${
        extension.version
      }${
        extension.setVersion === ExtensionSetVersion.EXPLICIT_IMPORT ? "" : `, default starting ${extension.setVersion}`
      }`,
    };
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
    return (
      ExtensionHandler.EXT_MAP.get(this.finalQuery) ||
      // A fall through case, ideally this should not happen.
      HoogleHandler.HOOGLE_BASE_URL + this.finalQuery
    );
  }

  removeExtensionPrefix(input: string): string {
    return this.removeTriggerPrefix(input, ExtensionHandler.TRIGGER_PREFIX);
  }

  giveSuggestions(input: string): chrome.omnibox.SuggestResult[] {
    const query = this.removeExtensionPrefix(input);
    this.parsePageAndRemovePager(query);
    const startCount = this.curPage * this.PAGE_SIZE;
    const endCount = startCount + this.PAGE_SIZE;
    let suggestExtData: ExtensionData[] = fuzzysort
      .go(this.finalQuery, extensionData, { key: "name", all: true })
      .map((x) => x.obj);
    this.totalPage = Math.ceil(suggestExtData.length / this.PAGE_SIZE);
    const suggestions = suggestExtData.slice(startCount, endCount).map(ExtensionHandler.extensionToSuggestResult);
    return suggestions;
  }

  adjustSuggestions(suggestions: chrome.omnibox.SuggestResult[], cache: SearchCache): void {
    const head = suggestions.shift();
    if (head) {
      cache.defaultContent = head.content;
      chrome.omnibox.setDefaultSuggestion({
        description: head.description + this.pageMessage(),
      });
    } else {
      // If the suggestion list is empty, try to redirect to hoogle.
      const hoogle = HoogleHandler.buildHoogleSuggestResult(this.finalQuery);
      chrome.omnibox.setDefaultSuggestion({
        description: hoogle.description,
      });
      cache.defaultContent = cache.currentInput;
      suggestions = [hoogle];
    }
  }
}
