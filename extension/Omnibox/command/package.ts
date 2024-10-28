import fuzzysort from "fuzzysort";
import { HackageData, hackageData } from "../data/hackage/hackageData";
import { CommandHandler, SearchCache } from "./type";
import { Compat } from "../Compat";
import HoogleHandler from "./hoogle";

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

  static packageToSuggestResult(data: HackageData): chrome.omnibox.SuggestResult {
    return {
      content: data.name,
      description: data.description,
    };
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
    this.parsePageAndRemovePager(query);
    const url = `https://hackage.haskell.org/package/${this.finalQuery}`;
    return url;
  }

  static searchResultToHackageData(highlightName: string, data: HackageData): HackageData {
    const desp = Compat.escape(data.description);
    const omniboxDescription = desp.length === 0 ? `${highlightName}` : `${highlightName} - ${desp}`;
    return new HackageData(data.name, omniboxDescription);
  }

  giveSuggestions(input: string): chrome.omnibox.SuggestResult[] {
    const query = this.removeExtensionPrefix(input);
    this.parsePageAndRemovePager(query);
    const startCount = this.curPage * this.PAGE_SIZE;
    const endCount = startCount + this.PAGE_SIZE;
    const suggestHackageData = fuzzysort.go(this.finalQuery, this.searchTargets, { key: "name", all: true });
    this.totalPage = Math.ceil(suggestHackageData.length / this.PAGE_SIZE);

    const suggestions: chrome.omnibox.SuggestResult[] = suggestHackageData
      .slice(startCount, endCount)
      // TODO: move these two map to `searchResultToHackageData`
      .map((x) => {
        const name = x.highlight("<match>", "</match>");
        return PackageHandler.searchResultToHackageData(name, new HackageData(x.obj.name.target, x.obj.description));
      })
      .map(PackageHandler.packageToSuggestResult);

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
    } else {
      // If the suggestion list is empty, try to redirect to hoogle.
      const hoogle = HoogleHandler.buildHoogleSuggestResult(this.finalQuery);
      chrome.omnibox.setDefaultSuggestion({ description: hoogle.description });
      cache.defaultContent = cache.currentInput;
      suggestions = [hoogle];
    }
  }

  removeExtensionPrefix(input: string): string {
    return this.removeTriggerPrefix(input, PackageHandler.TRIGGER_PREFIX);
  }
}
