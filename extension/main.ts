import { Compat } from "../core/compat"
import { Omnibox } from "../core/omnibox"
import { OmniboxFunctions, ResultType, SearchResult } from "../core/types"
import { HackageSearcher } from "./search/hackage"
import { data as cabal } from "./data/hackage"
import { data as hoogle } from "./data/hoogle"
import { processCabalData, processHoogleData } from "./util"

(async () => {
    console.log("load at: ", Date.now())
    const defaultSuggestionDesc = "Incredible search :)"
    const omnibox = new Omnibox(defaultSuggestionDesc, Compat.omniboxPageSize())
    const hackageSearcher = new HackageSearcher(processCabalData(cabal), processHoogleData(hoogle))

    const omniboxFuncs: OmniboxFunctions = {
        onSearch: (query) => {
            return hackageSearcher.search(query)
        },
        onFormat: (_, result: SearchResult): SearchResult => {
            switch (result.resultType) {
                case ResultType.Package: return {
                    content: `https://hackage.haskell.org/package/${result.path}`,
                    description: `[package] ${Compat.taggedMatch(Compat.escape(result.content))} - ${Compat.taggedDim(result.description)}`,
                    resultType: result.resultType
                    }
                case ResultType.HoogleFunc: return {
                    content: `https://hackage.haskell.org/package/${result.path}`,
                    description: `[${result.data ? result.data : "func"}] ${Compat.taggedMatch(Compat.escape(result.content))} - ${Compat.taggedDim(result.description)}`,
                    resultType: result.resultType
                }
                case undefined: throw "onFormat: impossible"
            }
        },
        onAppend: function (param: string): void {

        }
    }
    omnibox.bootstrap(omniboxFuncs)
})()
