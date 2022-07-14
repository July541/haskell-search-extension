import Command from "./base"
import "../compat"
import { OmniboxParam, OmniboxContent } from "../types"

/**
 * A simple command to quick setup a list item of [name, url, description] data search.
 */
class SimpleCommand extends Command {
    public params: OmniboxParam[]

    constructor(name: string, description: string, params: OmniboxParam[]) {
        super(name, description)
        this.params = params
    }

    async onExecute(arg?: string): Promise<OmniboxContent[]> {
        return this
            .params
            .filter(item => !arg || item.name.toLowerCase().indexOf(arg) > -1)
            .map(item => {
                if (item.description) {
                    item.description =
                        `${Compat.taggedMatch(item.name)} - ${Compat.taggedDim(Compat.escape(item.description))}`
                } else {
                    item.description = `${Compat.taggedMatch(item.name)} - ${Compat.taggedDim(item.url)}`
                }
                return {
                    content: item.url,
                    description: item.description
                }
            })
    }
}
