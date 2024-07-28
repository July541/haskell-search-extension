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
    return (
      ExtensionHandler.EXT_MAP.get(query) ||
      // A fall through case, ideally this should not happen.
      HoogleHandler.HOOGLE_BASE_URL + query
    );
  }

  removeExtensionPrefix(input: string): string {
    return this.removeTriggerPrefix(input, ExtensionHandler.TRIGGER_PREFIX);
  }

  giveSuggestions(input: string): chrome.omnibox.SuggestResult[] {
    const query = this.removeExtensionPrefix(input);
    const page = this.parsePage(input);
    const startCount = page * this.PAGE_SIZE;
    const endCount = startCount + this.PAGE_SIZE;
    let suggestExtData: ExtensionData[] = fuzzysort
      .go(query, extensionData, { key: "name", all: true })
      .map((x) => x.obj)
      .slice(startCount, endCount);
    const suggestions = suggestExtData.map(ExtensionHandler.extensionToSuggestResult);
    return suggestions;
  }

  adjustSuggestions(suggestions: chrome.omnibox.SuggestResult[], cache: SearchCache): void {
    const head = suggestions.shift();
    if (head) {
      cache.defaultContent = head.content;
      chrome.omnibox.setDefaultSuggestion({ description: head.description });
    } else {
      // If the suggestion list is empty, try to redirect to hoogle.
      const hoogle = HoogleHandler.buildHoogleSuggestResult(this.removeExtensionPrefix(cache.currentInput));
      chrome.omnibox.setDefaultSuggestion({ description: hoogle.description });
      cache.defaultContent = cache.currentInput;
      suggestions = [hoogle];
    }
  }
}
