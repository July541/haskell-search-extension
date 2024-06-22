export class Hoogle {

    static HOOGLE_BASE_URL: string = "https://hoogle.haskell.org/?hoogle=";

    /**
     * Generate a hoogle search suggestion by user input.
     * @param query User input
     * @returns
     */
    public static hoogleSearch(query: string): chrome.omnibox.SuggestResult {
        return {
            content: `${Hoogle.HOOGLE_BASE_URL}${query}`,
            description: `Search ${query} on [hoogle.haskell.org]`
        };
    }

    public static isHoogleUrl(url: string): boolean {
        return url.startsWith(Hoogle.HOOGLE_BASE_URL);
    }
}
