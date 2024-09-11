import fuzzysort from "fuzzysort";
import { HackageData, hackageData } from "../data/hackage/hackageData";
import { CommandHandler, SearchCache } from "./type";
import { Compat } from "../Compat";

class PreparedHackageData {
  name: Fuzzysort.Prepared;
  description: string;

  constructor(data: HackageData) {
    this.name = fuzzysort.prepare(data.name);
    this.description = data.description;
  }
}

export default class PackageHandler extends CommandHandler {
  private searchTargets = hackageData.map((x) => new PreparedHackageData(x));
  public static TRIGGER_PREFIX: string = ":pkg";

  public static isPackageMode(input: string): boolean {
    return this.hasTriggerPrefix(input, PackageHandler.TRIGGER_PREFIX);
  }

  handleChange(input: string, cache: SearchCache): chrome.omnibox.SuggestResult[] {
    const suggestions = this.giveSuggestions(input);
    this.adjustSuggestions(suggestions, cache);
    return suggestions;
  }

  handleEnter(input: string, cache: SearchCache): string {
    if (input === cache.currentInput) {
      // If the input is the same as the this.currentInput,
      // that means the user wants to use the first search result.
      // So we need to use the default content as the search target(like package name)
      input = cache.defaultContent;
    }
    const query = this.removeExtensionPrefix(input);
    const url = `https://hackage.haskell.org/package/${query}`;
    return url;
  }

  giveSuggestions(input: string): chrome.omnibox.SuggestResult[] {
    let query = this.removeExtensionPrefix(input);
    [this.curPage, query] = this.parsePage(query);
    const startCount = this.curPage * this.PAGE_SIZE;
    const endCount = startCount + this.PAGE_SIZE;

    const suggestHackageData: HackageData[] = fuzzysort
      .go(query, this.searchTargets, { key: "name", all: true })
      .map((x) => {
        const name = x.highlight("<match>", "</match>");
        const desp = Compat.escape(x.obj.description);
        const omniboxDescription = desp.length === 0 ? `[package] ${name}` : `[package] ${name} - ${desp}`;
        return new HackageData(x.obj.name.target, omniboxDescription);
      });

    this.totalPage = Math.ceil(suggestHackageData.length / this.PAGE_SIZE);

    const suggestions: chrome.omnibox.SuggestResult[] = suggestHackageData
      .slice(startCount, endCount)
      .map((x: HackageData) => ({
        content: x.name,
        description: x.description,
      }));

    return suggestions;
  }

  /**
   * Cache the content of the first suggestion and set it as the chrome's default suggestion.
   * Otherwise there will left the user input in the first line of the suggestion result.
   * @param suggestions
   */
  adjustSuggestions(suggestions: chrome.omnibox.SuggestResult[], cache: SearchCache) {
    const head = suggestions.shift();
    if (head) {
      // Save the content of the first suggestion, so that we can recover it
      // if the user select the first suggestion while entering.
      cache.defaultContent = head.content;
      chrome.omnibox.setDefaultSuggestion({ description: head.description + this.pageMessage() });
    }
  }

  removeExtensionPrefix(input: string): string {
    return this.removeTriggerPrefix(input, PackageHandler.TRIGGER_PREFIX);
  }
}
