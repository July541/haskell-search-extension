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
     * @param arg
     * @param callback
     */
    onEnter(content: string, callback: any): void {}

    /**
     * A hook method called when the onExecute()'s result is empty.
     * @param arg
     */
    onBlankResult(arg: any): string[] {
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