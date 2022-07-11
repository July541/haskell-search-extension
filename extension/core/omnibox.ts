/**
 * Used to go to the next page with one `PAGE_TURNER`
 */
const PAGE_TURNER = "-";
const URL_PROTOCOLS = /^(https?|file|chrome-extension|moz-extension):\/\//i;

class Omnibox {
    private maxSuggestionSize: number
    private defaultSuggestionDescriptions: string
    private defaultSuggestionContent?: string
    private queryEvents: any
    /**
     * Cache the latest query and its result to speed up.
     */
    private cached?: CachedQuery
    private noCacheQueries: Set<string>

    private globalEvent: QueryEvent

    constructor(defaultSuggestionDesc: string, maxSuggestionSize: number = 8) {
        this.maxSuggestionSize = maxSuggestionSize
        this.defaultSuggestionDescriptions = defaultSuggestionDesc
        this.defaultSuggestionContent = undefined
        this.queryEvents = []
        this.cached = undefined
        this.noCacheQueries = new Set()
    }

    setDefaultSuggestion(description: string, content?: string): void {
        chrome.omnibox.setDefaultSuggestion({ description })

        this.defaultSuggestionContent = content
    }

    parse(input: string): QueryWithPage {
        let parsePage = (arg: string) => {
            // plus one to start from page 2
            return [...arg].filter(c => c === PAGE_TURNER).length + 1
        }
        let args: string[] = input.trim().split(/\s+/i)
        let query: string[] = []
        let page = 1

        if (args.length === 1) {
            // {keyword}
            query = [args[0]]
        } else if (args.length === 2 && args[1].startsWith(PAGE_TURNER)) {
            // {keyword} {page-turner}
            query = [args[0]]
            page = parsePage(args[1])
        } else if (args.length > 2) {
            query = [args[0], args[1]]
            if (args[2] && args[2].startsWith(PAGE_TURNER)) {
                page = parsePage(args[2])
            }
            if (args.length > 3) {
                console.warn("Unexpected args: ", args)
            }
        }
        return { query: query.join(" "), page }
    }

    bootstrap(funcs: OmniboxFunctions): void {
        this.globalEvent = new QueryEvent({ onSearch: funcs.onSearch })
        this.setDefaultSuggestion(this.defaultSuggestionDescriptions)

        let results
        let currentInput
        let defaultDescription

        chrome.omnibox.onInputChanged.addListener(async (input: string, suggestFn) => {
            // Set the default suggestion content to input instead null,
            // this could prevent content null bug in onInputEntered().
            this.defaultSuggestionContent = input;

            if (!input) {
                this.setDefaultSuggestion(this.defaultSuggestionDescriptions)
                return
            }

            currentInput = input
            let {query, page} = this.parse(input)
            if (this.noCacheQueries.has(query) || this.cached?.queryInput !== query) {
                // results = await this.per
            }

        })
    }
}