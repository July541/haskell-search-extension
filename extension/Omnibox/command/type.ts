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
}

export abstract class CommandHandler {
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
   * 0 based page number
   * @param input
   * @returns
   */
  parsePage(input: string): number {
    return 0;
  }

  /**
   * Check if the input has a trigger prefix.
   * @param input The user input.
   * @param prefixes Trigger prefixes.
   * @returns
   */
  static hasTriggerPrefix(input: string, ...prefixes: string[]): boolean {
    // We need to add a space after the prefix to avoid the ambiguity.
    return prefixes.some((prefix) => input.startsWith(prefix + " "));
  }

  /**
   * Remove the trigger prefix from the input.
   * @param input The user input.
   * @param prefixes Trigger prefixes.
   */
  removeTriggerPrefix(input: string, ...prefixes: string[]): string {
    for (const prefix of prefixes) {
      if (input.startsWith(prefix + " ")) {
        // Note that we need to add a space after the prefix.
        return input.slice(prefix.length + 1).trim();
      }
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
