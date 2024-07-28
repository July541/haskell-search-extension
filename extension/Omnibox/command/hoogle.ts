import { Compat } from "../Compat";
import { CommandHandler, SearchCache } from "./type";

export default class HoogleHandler extends CommandHandler {
  private static HOOGLE_URL: string = "https://hoogle.haskell.org";
  public static HOOGLE_BASE_URL: string = `${this.HOOGLE_URL}/?hoogle=`;

  public static TRIGGER_PREFIXES: string[] = [":hoogle", ":hg"];

  private static HOOGLE_DEFAULT_DESCRIPTION: string = "Continue typing or press entering to hoogle.com";

  public static isHoogleMode(input: string): boolean {
    return this.hasTriggerPrefix(input, ...this.TRIGGER_PREFIXES);
  }

  public static isHoogleUrl(input: string): boolean {
    return input.startsWith(HoogleHandler.HOOGLE_BASE_URL);
  }

  /**
   * Generate a hoogle search suggestion by user input, note that the
   * input is not prefixed with `:hoogle` or `:hg`.
   * @param query
   * @returns
   */
  public static buildHoogleSuggestResult(query: string): chrome.omnibox.SuggestResult {
    return {
      content: HoogleHandler.HOOGLE_BASE_URL + query,
      description: `Search ${Compat.taggedMatch(query)} on [hoogle.haskell.org]`,
    };
  }

  handleChange(input: string, _cache: SearchCache): chrome.omnibox.SuggestResult[] {
    const suggestions = this.giveSuggestions(input);
    const head = suggestions.shift();
    if (head) {
      chrome.omnibox.setDefaultSuggestion({ description: head.description });
    }
    // We don't need to show any suggestions in the dropdown list,
    // since we have added a default suggestion.
    // Here the `suggestions` should always be empty.
    return suggestions;
  }

  giveSuggestions(input: string): chrome.omnibox.SuggestResult[] {
    const query = this.removeHooglePrefix(input);
    console.log(query);
    if (query.length === 0) {
      return [{ content: HoogleHandler.HOOGLE_URL, description: HoogleHandler.HOOGLE_DEFAULT_DESCRIPTION }];
    }
    return [HoogleHandler.buildHoogleSuggestResult(query)];
  }

  handleEnter(input: string, _cache: SearchCache): string {
    return HoogleHandler.HOOGLE_BASE_URL + this.removeHooglePrefix(input);
  }

  removeHooglePrefix(input: string): string {
    return this.removeTriggerPrefix(input, ...HoogleHandler.TRIGGER_PREFIXES);
  }
}
