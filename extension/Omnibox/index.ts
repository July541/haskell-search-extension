import { hackageData, HackageData } from "./hackageData";
import fuzzysort from 'fuzzysort';
import { Compat } from "./Compat";
import { Hoogle } from "./hoogle";

class PreparedHackageData {
    name: Fuzzysort.Prepared
    description: string

    constructor(data: HackageData) {
        this.name = fuzzysort.prepare(data.name);
        this.description = data.description;
    }
}

const searchTargets = hackageData.map(x => new PreparedHackageData(x));

const PAGE_SIZE: number = 10;

export class Omnibox {
    // Unused, I'm not sure it's time to optimize
    // cacheResults: CacheResults = new Map();

    /**
     * 0 based
     */
    parsePage(input: string): number {
        return 0;
    }

    bootstrap() {
        chrome.omnibox.onInputChanged.addListener((input: string, suggest) => {
            const page = this.parsePage(input);
            const startCount = page * PAGE_SIZE;
            const endCount = startCount + PAGE_SIZE

            const res: HackageData[] = fuzzysort.go(input, searchTargets, { key: "name" })
                .map(x => new HackageData(x.target, x.obj.description))
                .slice(startCount, endCount);

            let suggestions: chrome.omnibox.SuggestResult[] = res
                .map((x: HackageData) => ({
                    content: x.name,
                    description: x.description.length == 0 ? `[package] ${Compat.escape(x.name)}` :
                        `[package] ${Compat.escape(x.name)} - ${Compat.escape(x.description)}`
                }));
            // insert hoogle search on the fiest position
            suggestions.unshift(Hoogle.hoogleSearch(input));

            if (suggestions.length > 0) {
                const defaultSuggestion = suggestions.shift();
                if (defaultSuggestion) {
                    chrome.omnibox.setDefaultSuggestion({ description: defaultSuggestion.description });
                }
            }

            suggest(suggestions);
        });

        chrome.omnibox.onInputEntered.addListener((input: string) => {
            let url = `https://hackage.haskell.org/package/${input}`;
            if (input.startsWith(Hoogle.HOOGLE_PREFIX)) {
                const query = input.substring(Hoogle.HOOGLE_PREFIX.length);
                url = `https://hoogle.haskell.org/?hoogle=${query}`;
            }
            chrome.tabs.update({ url });
        });
    }
}
