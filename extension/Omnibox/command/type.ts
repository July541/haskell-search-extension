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
    SearchHoogle
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
}

