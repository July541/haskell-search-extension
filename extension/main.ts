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
        onFormat: (_, result) => {
            let { ...ret } = result
            ret.path = `https://hackage.haskell.org/package/${result.content}`
            ret.description = `[package] ${Compat.taggedMatch(Compat.escape(result.content))} - ${Compat.taggedDim(Compat.escape(result.description))}`
            return ret
        },
        onAppend: function (param: string): void {

        }
    }
    omnibox.bootstrap(omniboxFuncs)
})()
