export class SearchCache {
  /**
   * We save the original input here in case the user wants to use the default content.
   * See package.ts for detailed usage.
   */
  public defaultContent: string = "";

  /**
   * Store the current input for `OnInputChanged` event,
   * and when `OnInputEntered` event is fired, it will be used to determine whether to use the default content.
   *
   * We can use this rely on the fact that through up and down arrow keys, the `OnInputChanged` will not be fired.
   */
  public currentInput: string = "";
}

export enum Command {
  SearchPackage,
  SearchHoogle,
  SearchExtension,
  SearchDefault,
  SearchMeta,
  SearchLink,
  SearchError,
}

export abstract class CommandHandler {
  curPage: number = 0;
  totalPage: number = 0;
  finalQuery: string = "";

  /**
   * Generate suggestions by input.
   * @param input The search input by the user.
   * @param cache The cache object, see package.ts for detailed usage.
   * @returns The search result will be displayed in the chrome omnibox.
   */
  abstract handleChange(input: string, cache: SearchCache): chrome.omnibox.SuggestResult[];

  /**
   * Handle the enter event.
   * @param input The search input while user press enter.
   * @param cache The cache object, see package.ts for detailed usage.
   */
  abstract handleEnter(input: string, cache: SearchCache): string;

  public PAGE_SIZE: number = 10;

  /**
   * Parse the page number from the input.
   * The input should not contains any trigger prefix.
   *
   * **The function will also set the finalQuery field.**
   *
   * @example parsePage("arr -") => [1, "arr"]
   * @example parsePage("arr --") => [2, "arr"]
   * @example parsePage("arr") => [0, "arr"]
   * @example parsePage("arr-1") => [0, "arr-1"]
   * @example parsePage("arr-") => [0, "arr-"]
   * @param input
   */
  parsePageAndRemovePager(input: string) {
    let cnt = 0;
    const rev = [...input].reverse();
    const page_sep = "-";
    for (const c of rev) {
      if (c === page_sep) {
        cnt++;
      } else if (c === " ") {
        break;
      } else {
        // If the current character is not a '-' or a space, it indicate that
        // the '-' is the query part, not the page part.
        // return 0;
        cnt = 0;
        break;
      }
    }
    this.finalQuery = rev.slice(cnt).reverse().join("").trim();
    this.curPage = cnt;
  }

  pageMessage(): string {
    const fixedPage = this.curPage >= this.totalPage ? this.totalPage : this.curPage + 1;
    return ` | Page [${fixedPage}/${this.totalPage}], append '-' to page down`;
  }

  /**
   * Check if the input has a trigger prefix.
   * @param input The user input.
   * @param prefixes Trigger prefixes.
   * @returns
   */
  static hasTriggerPrefix(input: string, prefix: string): boolean {
    return input.startsWith(prefix);
  }

  /**
   * Remove the trigger prefix from the input.
   * @param input The user input.
   * @param prefixes Trigger prefixes.
   */
  removeTriggerPrefix(input: string, prefix: string): string {
    if (input.startsWith(prefix + " ")) {
      // Note that we need to add a space after the prefix.
      return input.slice(prefix.length + 1).trim();
    } else if (input.startsWith(prefix)) {
      return input.slice(prefix.length).trim();
    }
    return input;
  }

  /**
   * Give corresponding suggestions by the input.
   * This function should return the all suggestion without adjusting for omnibox display,
   * and this function should always be called in `handleChange`, and then adjust the suggestions
   * in `handleChange`.
   *
   * We extract function mainly for testing purpose.
   * @param input The **original** user input without any modification.
   */
  abstract giveSuggestions(input: string): chrome.omnibox.SuggestResult[];
}
