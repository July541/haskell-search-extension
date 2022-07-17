import { Compat } from "../core/compat"
import { Omnibox } from "../core/omnibox"
import { OmniboxFunctions } from "../core/types"
import { HackageSearcher } from "./search/hackage"
import { data } from "./data/hackage"

(async () => {
    console.log("ok")
    const defaultSuggestionDesc = "Incredible search :)"
    const omnibox = new Omnibox(defaultSuggestionDesc, Compat.omniboxPageSize())
    const hackageSearcher = new HackageSearcher(data)

    const omniboxFuncs: OmniboxFunctions = {
        onSearch: (query) => {
            return hackageSearcher.search(query)
        },
        onAppend: function (param: string): void {
            
        }
    }
    omnibox.bootstrap(omniboxFuncs)
})()