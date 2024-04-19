import { hackageData, HackageData } from "./hackageData";
import Fuse from 'fuse.js';
import { Compat } from "./Compat";

const fuse = new Fuse(hackageData, { keys: ['name'] });

type CacheResults = Map<string, chrome.omnibox.SuggestResult[]>;

export class Omnibox {
    cacheResults: CacheResults = new Map();

    bootstrap() {
        chrome.omnibox.onInputChanged.addListener((input: string, suggest) => {
            // const cache = this.cacheResults.get(input);
            // if (cache) {
            //     suggest(cache);
            //     return;
            // }

            const res = fuse.search(input).map(x => x.item);
            const suggestions = res.map((x: HackageData) => ({
                content: x.name,
                description: x.description.length == 0 ? `[package] ${Compat.escape(x.name)}` :
                    `[package] ${Compat.escape(x.name)} - ${Compat.escape(x.description)}`
            }));
            // console.log(suggestions);
            // this.cacheResults.set(input, suggestions);
            suggest(suggestions);
        });

        chrome.omnibox.onInputEntered.addListener((input: string) => {
            const url = `https://hackage.haskell.org/package/${input}`;
            chrome.tabs.update({ url });
        });
    }
}
