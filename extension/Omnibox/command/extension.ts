import fuzzysort from "fuzzysort";
import { ExtensionData, extensionData, IncludedVersion } from "../data/extension/extensionData";
import { CommandHandler, SearchCache } from "./type";
import HoogleHandler from "./hoogle";

export default class ExtensionHandler extends CommandHandler {
  public static TRIGGER_PREFIX: string = ":ext";
  private static EXT_MAP: Map<string, string> = new Map(extensionData.map((x) => [x.name, x.url]));
  private static EXT_BASE_URL: string = "https://ghc.gitlab.haskell.org/ghc/doc/users_guide/";
  static Haskell98_Included = extensionData.filter((x) => x.included.includes(IncludedVersion.Haskell98));
  static Haskell2010_Included = extensionData.filter((x) => x.included.includes(IncludedVersion.Haskell2010));
  static GHC2021_Included = extensionData.filter((x) => x.included.includes(IncludedVersion.GHC2021));
  static GHC2024_Included = extensionData.filter((x) => x.included.includes(IncludedVersion.GHC2024));

  public static isExtensionMode(input: string): boolean {
    return this.hasTriggerPrefix(input, this.TRIGGER_PREFIX);
  }

  private static extensionToSuggestResult(extension: ExtensionData): chrome.omnibox.SuggestResult {
    const deprecatedStr = extension.deprecated ? "[deprecated] " : "";
    const nameStr = `{-# LANGUAGE ${extension.name} #-} `;
    const sinceStr = extension.since ? `Since ${extension.since}` : "";
    const includedStr =
      extension.included.length === 0 || extension.included[0] === "NA"
        ? ""
        : `Included in ${extension.included.join(",")}`;
    return {
      content: ExtensionHandler.TRIGGER_PREFIX + " " + extension.name, // Add <:ext > prefix to make sure that the `handleChange` works.
      description: `${deprecatedStr}${nameStr}${sinceStr}${includedStr.length === 0 ? "" : " "}${includedStr}`,
    };
  }

  /**
   * Some extensions like `GHC2024` enabled some other extensions by default(`DataKinds` for example),
   * so we'd like to apply these enabled extensions if the user types like `GHC2024`.
   * @param suggestions
   * @returns
   */
  expandSuggestionForIncluded(suggestions: ExtensionData[]) {
    if (suggestions.length === 0) {
      return;
    }

    const head = suggestions[0];
    const isValidIncludedVersion = Object.values(IncludedVersion)
      .map((x) => x.toString())
      .includes(head.name);

    if (!isValidIncludedVersion || head.name === "NA") {
      return;
    }

    const includedExtensions = extensionData.filter((x) => x.included.includes(head.name as IncludedVersion));
    const suggestionNames = suggestions.map((x) => x.name);
    includedExtensions.forEach((x) => {
      if (!suggestionNames.includes(x.name)) {
        suggestions.push(x);
      }
    });
  }

  handleChange(input: string, cache: SearchCache): chrome.omnibox.SuggestResult[] {
    const suggestions = this.giveSuggestions(input);
    this.adjustSuggestions(suggestions, cache);
    return suggestions;
  }

  handleEnter(input: string, cache: SearchCache): string {
    const query = this.removeExtensionPrefix(input);
    this.parsePageAndRemovePager(query);
    return (
      ExtensionHandler.EXT_BASE_URL + ExtensionHandler.EXT_MAP.get(this.finalQuery) ||
      // A fall through case, ideally this should never happen.
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
    const suggestExtData: ExtensionData[] = fuzzysort
      .go(this.finalQuery, extensionData, { key: "name", all: true })
      .map((x) => x.obj);
    this.expandSuggestionForIncluded(suggestExtData);
    this.buildPageInfo(suggestExtData.length);
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
      cache.defaultContent = HoogleHandler.TRIGGER_PREFIX + " " + this.removeExtensionPrefix(cache.currentInput);
      suggestions = [hoogle];
    }
  }
}
