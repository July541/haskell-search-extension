import fuzzysort from "fuzzysort";
import ExtensionHandler from "./extension";
import HoogleHandler from "./hoogle";
import PackageHandler from "./package";
import { CommandHandler, SearchCache } from "./type";
import { Compat } from "../Compat";

export default class MetaHandler extends CommandHandler {
  private static TRIGGER_PREFIXES: string[] = [":"];

  private static META_ITEMS: string[] = [
    ...PackageHandler.TRIGGER_PREFIXES,
    ...ExtensionHandler.TRIGGER_PREFIXES,
    ...HoogleHandler.TRIGGER_PREFIXES,
  ];

  public static isMetaMode(input: string): boolean {
    return this.hasTriggerPrefix(input, ...this.TRIGGER_PREFIXES);
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
    if (PackageHandler.TRIGGER_PREFIXES.includes(key)) {
      return "Search Hackage packages.";
    } else if (ExtensionHandler.TRIGGER_PREFIXES.includes(key)) {
      return "Search Haskell language extensions";
    } else if (HoogleHandler.TRIGGER_PREFIXES.includes(key)) {
      return "Search on hoogle";
    }
    return "Unexpected command: " + key;
  }
}
