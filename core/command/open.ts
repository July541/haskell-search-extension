import Command from "./base"
import { Omnibox } from "../omnibox"

type Result = chrome.omnibox.SuggestResult

/**
 * A command simply to quick open the specific url.
 */
class OpenCommand extends Command {
    public url: string
    public blankResult: Result

    /**
     *
     * @param name The command name. (without command prefix)
     * @param description The command description.
     * @param url The url to open
     * @param blankResult The blankResult is a object: {content, description}.
     */
    constructor(name: string, description: string, url: string, blankResult: Result) {
        super(name, description)
        this.url = url
        this.blankResult = blankResult
    }

    onEnter(content: string, disposition: chrome.omnibox.OnInputEnteredDisposition): void {
        Omnibox.navigateToUrl(this.url, disposition)
    }

    onBlankResult(_: any): chrome.omnibox.SuggestResult[] {
        return [
            { content: this.blankResult.content
            , description: this.blankResult.description
            }
        ]
    }
}
