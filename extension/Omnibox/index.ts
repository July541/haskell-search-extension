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

    defaultContent: string = "";

    /**
     * Store the current input for `OnInputChanged` event,
     * and when `OnInputEntered` event is fired, it will be used to determine whether to use the default content.
     *
     * We can use this rely on the fact that through up and down arrow keys, the `OnInputChanged` will not be fired.
     */
    currentInput: string = "";

    /**
     * 0 based
     */
    parsePage(input: string): number {
        return 0;
    }

    bootstrap() {
        chrome.omnibox.onInputChanged.addListener((input: string, suggest) => {
            this.currentInput = input;

            const suggestions = this.searchPackage(input);
            suggest(suggestions);
        });

        chrome.omnibox.onInputEntered.addListener((input: string) => {
            if (input === this.currentInput) {
                // If the input is the same as the this.currentInput,
                // that means the user wants to use the first search result.
                // So we need to use the default content as the search target(like package name)
                input = this.defaultContent;
            }

            if (Hoogle.isHoogleUrl(input)) {
                chrome.tabs.update({ url: input });
                return;
            }

            let url = `https://hackage.haskell.org/package/${input}`;
            chrome.tabs.update({ url });
        });
    }

    searchPackage(input: string): chrome.omnibox.SuggestResult[] {
        const page = this.parsePage(input);
        const startCount = page * PAGE_SIZE;
        const endCount = startCount + PAGE_SIZE;
        const res: HackageData[] = fuzzysort
            .go(input, searchTargets, { key: "name" })
            .map(x => new HackageData(x.target, x.obj.description))
            .slice(startCount, endCount);

        const suggestions: chrome.omnibox.SuggestResult[] = res
            .map((x: HackageData) => ({
                content: x.name,
                description: x.description.length == 0
                    ? `[package] ${Compat.escape(x.name)}`
                    : `[package] ${Compat.escape(x.name)} - ${Compat.escape(x.description)}`
            }));

        this.coreceWithHoogle(suggestions, input);
        this.adjustSuggestions(suggestions);

        return suggestions;
    }

    coreceWithHoogle(suggestions: chrome.omnibox.SuggestResult[], input: string) {
        const head = suggestions.shift();
        suggestions.unshift(Hoogle.hoogleSearch(input));

        if (head) {
            suggestions.unshift(head);
        }
    }

    adjustSuggestions(suggestions: chrome.omnibox.SuggestResult[]) {
        const head = suggestions.shift();
        if (head) {
            // Save the content of the first suggestion, so that we can recover it
            // if the user select the first suggestion while entering.
            this.defaultContent = head.content;
            chrome.omnibox.setDefaultSuggestion({ description: head.description });
        }
    }
}
