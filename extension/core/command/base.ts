class Command {
    public name: string
    public description: string

    constructor(name: string, description: string) {
        this.name = name;
        this.description = description;
    }

    async onExecute(arg: any): Promise<void> {}

    /**
     * A hook method called when press enter on command directly.
     * @param content
     * @param disposition
     */
    onEnter(content: string, disposition: chrome.omnibox.OnInputEnteredDisposition): void {}

    /**
     * A hook method called when the onExecute()'s result is empty.
     * @param arg
     */
    onBlankResult(arg: any): chrome.omnibox.SuggestResult[] {
        return []
    }

    wrapCommand(result: string[]): OmniboxContent[] {
        return result.map((desc, index) => {
            return {
                content: `${index + 1}`,
                description: desc
            }
        })
    }
}
