import { CommandHandler, SearchCache } from "./type";

export default class HoogleHandler extends CommandHandler {
    private static HOOGLE_PREFIX_1: string = ":hoogle";
    private static HOOGLE_PREFIX_2: string = ":hg";
    private static HOOGLE_BASE_URL: string = "https://hoogle.haskell.org/?hoogle=";


    public static isHoogleMode(input: string): boolean {
        return input.startsWith(HoogleHandler.HOOGLE_PREFIX_1)
            || input.startsWith(HoogleHandler.HOOGLE_PREFIX_2);
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
            description: `Search ${query} on [hoogle.haskell.org]`
        };
    }

    handleChange(input: string, _cache: SearchCache): chrome.omnibox.SuggestResult[] {
        const query = this.removeHooglePrefix(input);
        const result = HoogleHandler.buildHoogleSuggestResult(query);
        chrome.omnibox.setDefaultSuggestion({ description: result.description });

        // We don't need to show any suggestions in the dropdown list,
        // since we have add a default suggestion.
        return [];
    }

    handleEnter(input: string, _cache: SearchCache): string {
        return HoogleHandler.HOOGLE_BASE_URL + this.removeHooglePrefix(input);
    }

    removeHooglePrefix(input: string): string {
        if (input.startsWith(HoogleHandler.HOOGLE_PREFIX_1)) {
            return input.slice(HoogleHandler.HOOGLE_PREFIX_1.length).trim();
        } else if (input.startsWith(HoogleHandler.HOOGLE_PREFIX_2)) {
            return input.slice(HoogleHandler.HOOGLE_PREFIX_2.length).trim();
        }
        return input;
    }
}