import fuzzysort from "fuzzysort";
import { CommandHandler, SearchCache } from "./type";
import { HackageData, hackageData } from "../data/hackage/hackageData";
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
  handleChange(input: string, cache: SearchCache): chrome.omnibox.SuggestResult[] {
    const page = this.parsePage(input);
    const startCount = page * this.PAGE_SIZE;
    const endCount = startCount + this.PAGE_SIZE;

    const suggestHackageData: HackageData[] = fuzzysort
      .go(input, this.searchTargets, { key: "name" })
      .map((x) => new HackageData(x.target, x.obj.description))
      .slice(startCount, endCount);

    const suggestions: chrome.omnibox.SuggestResult[] = suggestHackageData.map((x: HackageData) => ({
      content: x.name,
      description:
        x.description.length == 0
          ? `[package] ${Compat.escape(x.name)}`
          : `[package] ${Compat.escape(x.name)} - ${Compat.escape(x.description)}`,
    }));

    this.coreceWithHoogle(suggestions, input);
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

    // If the suggestion list is not empty, that means the user select hoogle
    // by pressing up and down arrow keys, that promise the input must be a hoogle url.
    // If the suggestion list is empty, function `adjustSuggestions` will save the
    // hoogle url to cache.defaultContent, since there is no other suggestion,
    // the input must equal to cache.currentInput, that will make the input be
    // assigned to cache.defaultContent, in which case the input will be a hoogle url.
    //
    // Note that there has an assumption that modifying the content of the
    // `chrome.omnibox.SuggestionResult` won't affect the user's current input
    // in the omnibox.
    if (HoogleHandler.isHoogleUrl(input)) {
      return input;
    }

    const url = `https://hackage.haskell.org/package/${input}`;
    return url;
  }

  /**
   * Coerce the suggestions with hoogle search.
   * @param suggestions Existed suggestions
   * @param input The user input
   */
  coreceWithHoogle(suggestions: chrome.omnibox.SuggestResult[], input: string) {
    const head = suggestions.shift();
    suggestions.unshift(HoogleHandler.buildHoogleSuggestResult(input));

    if (head) {
      suggestions.unshift(head);
    }
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
      chrome.omnibox.setDefaultSuggestion({ description: head.description });
    }
  }
}
