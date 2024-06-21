export class Hoogle {

    static HOOGLE_BASE_URL: string = "https://hoogle.haskell.org/?hoogle=";

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
