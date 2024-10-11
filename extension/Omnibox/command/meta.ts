import fuzzysort from "fuzzysort";
import ExtensionHandler from "./extension";
import HoogleHandler from "./hoogle";
import PackageHandler from "./package";
import { CommandHandler, SearchCache } from "./type";
import { Compat } from "../Compat";
import LinkHandler from "./link";
import ErrorHandler from "./error";

export default class MetaHandler extends CommandHandler {
  private static TRIGGER_PREFIX: string = ":";

  private static META_ITEMS: string[] = [
    PackageHandler.TRIGGER_PREFIX,
    ExtensionHandler.TRIGGER_PREFIX,
    HoogleHandler.TRIGGER_PREFIX,
    LinkHandler.TRIGGER_PREFIX,
    ErrorHandler.TRIGGER_PREFIX,
  ];

  public static isMetaMode(input: string): boolean {
    return this.hasTriggerPrefix(input, this.TRIGGER_PREFIX);
  }

  handleChange(input: string, cache: SearchCache): chrome.omnibox.SuggestResult[] {
    const suggestions = this.giveSuggestions(input);
    chrome.omnibox.setDefaultSuggestion({
      description: "Found following commands, press Tab to select.",
    });
    return suggestions;
  }

  handleEnter(input: string, cache: SearchCache): string {
    return "";
  }

  giveSuggestions(input: string): chrome.omnibox.SuggestResult[] {
    const suggestions = fuzzysort.go(input, MetaHandler.META_ITEMS, { all: true });
    return suggestions.map((x) => ({
      content: x.target,
      description: Compat.taggedMatch(x.target) + " - " + this.commandDescription(x.target),
    }));
  }

  private commandDescription(key: string): string {
    if (key === PackageHandler.TRIGGER_PREFIX) {
      return "Search Hackage packages.";
    } else if (key === ExtensionHandler.TRIGGER_PREFIX) {
      return "Search Haskell language extensions";
    } else if (key === HoogleHandler.TRIGGER_PREFIX) {
      return "Search on hoogle";
    } else if (key === LinkHandler.TRIGGER_PREFIX) {
      return "Search useful links";
    } else if (key === ErrorHandler.TRIGGER_PREFIX) {
      return "Search Haskell errors";
    }
    return "Unexpected command: " + key;
  }
}
