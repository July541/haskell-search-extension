import { hackageData, HackageData } from "./hackageData";
import fuzzysort from 'fuzzysort';
import { Compat } from "./Compat";

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

            const suggestions: chrome.omnibox.SuggestResult[] = res
                .map((x: HackageData) => ({
                    content: x.name,
                    description: x.description.length == 0 ? `[package] ${Compat.escape(x.name)}` :
                        `[package] ${Compat.escape(x.name)} - ${Compat.escape(x.description)}`
                }));
            suggest(suggestions);
        });

        chrome.omnibox.onInputEntered.addListener((input: string) => {
            const url = `https://hackage.haskell.org/package/${input}`;
            chrome.tabs.update({ url });
        });
    }
}
