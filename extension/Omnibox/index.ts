import { hackageData, HackageData } from "./hackageData";
import Fuse from 'fuse.js';

const fuse = new Fuse(hackageData, {keys: ['name']});

export class Omnibox {
    bootstrap() {
        chrome.omnibox.onInputChanged.addListener((input: string, suggest) => {
            const res = fuse.search(input).map(x => x.item);
            const suggestions = res.map((x: HackageData) => ({content: x.name, description: x.description}));
            suggest(suggestions);
        });
    }
}
