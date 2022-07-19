import { Compat } from "../core/compat"
import { Omnibox } from "../core/omnibox"
import { OmniboxFunctions, SearchResult } from "../core/types"
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
        onFormat: (_, result): SearchResult => {
            return {
                content: `https://hackage.haskell.org/package/${result.path}`,
                description: `[package] ${Compat.taggedMatch(Compat.escape(result.path))} - ${Compat.taggedDim(Compat.escape(result.description))}`
            }
        },
        onAppend: function (param: string): void {

        }
    }
    omnibox.bootstrap(omniboxFuncs)
})()
