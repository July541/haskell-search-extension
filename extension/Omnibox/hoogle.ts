export class Hoogle {
    public static HOOGLE_PREFIX = "hoogle:";

    public static hoogleSearch(query: string): chrome.omnibox.SuggestResult {
        return {
            content: Hoogle.HOOGLE_PREFIX + query,
            description: `Search ${query} on [hoogle.haskell.org]`
        };
    }
}
