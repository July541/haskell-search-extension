import { CachedQuery } from "./cache";
import { QueryEvent } from "./query-event";
import { QueryWithPage, OmniboxFunctions, SearchResult, QueryEventFuncs } from "./types";

/**
 * Used to go to the next page with one `PAGE_TURNER`
 */
const PAGE_TURNER = "-";
const URL_PROTOCOLS = /^(https?|file|chrome-extension|moz-extension):\/\//i;

export class Omnibox {
    private maxSuggestionSize: number
    private defaultSuggestionDescriptions: string
    private defaultSuggestionContent?: string
    private queryEvents: QueryEvent[]
    /**
     * Cache the latest query and its result to speed up.
     */
    private cached?: CachedQuery
    private noCacheQueries: Set<string>

    private globalEvent?: QueryEvent

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
        } else if (args.length >= 2) {
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
        this.globalEvent = new QueryEvent({ onSearch: funcs.onSearch, onFormat: funcs.onFormat })
        this.setDefaultSuggestion(this.defaultSuggestionDescriptions)

        let results: SearchResult[]
        let currentInput: string
        let defaultDescription: string

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
                results = await this.doSearch(query)
                this.cached = { queryInput: query, queryResult: results }
            } else {
                results = this.cached.queryResult
            }

            let totalPage = Math.ceil(results.length / this.maxSuggestionSize)
            let uniqueUrls: Set<string> = new Set()

            results = results
                .slice(this.maxSuggestionSize * (page - 1), this.maxSuggestionSize * page)
                .map(({ event, ...item} , index) => {
                    if (event) {
                        item = event.doFormat(item, index)
                    }
                    if (uniqueUrls.has(item.content)) {
                        item.content += `?${uniqueUrls.size + 1}`
                    }
                    uniqueUrls.add(item.content)
                    return item
                })

            if (results.length > 0) {
                let { content, description } = results.shift()!
                defaultDescription = description
                description += ` | Page [${page}/${totalPage}], append '${PAGE_TURNER}' to page down`
                this.setDefaultSuggestion(description, content)
            }
            suggestFn(results)
        })

        chrome.omnibox.onInputEntered.addListener(async (content: string, disposition) => {
            let result: SearchResult | undefined
            // Give a default implemention if `beforeNavigate` is not available.
            if (!funcs.beforeNavigate) {
                funcs.beforeNavigate = (async (_, s) => s!) // Let crushed if s is undefined
            }

            // A flag indicates whether the url navigate success
            let navigated = false
            if (content == currentInput) {
                content = await funcs.beforeNavigate!(this.cached, this.defaultSuggestionContent)
                result = {
                    content,
                    description: defaultDescription
                }
                if (URL_PROTOCOLS.test(content)) {
                    Omnibox.navigateToUrl(content, disposition)
                    navigated = true
                }
            } else {
                // Store raw content before navigate to find the correct result
                let rawContent = content
                result = results.find(item => item.content === rawContent)
                content = await funcs.beforeNavigate(this.cached, content)
                if (URL_PROTOCOLS.test(content)) {
                    Omnibox.navigateToUrl(content, disposition)
                    navigated = true

                    if (result) {
                        result.content = content
                    }
                }
            }

            if (navigated && funcs.afterNavigate) {
                await funcs.afterNavigate(this.cached, result)
            } else if (funcs.onEmptyNavigate) {
                await funcs.onEmptyNavigate(content, disposition)
            }

            this.setDefaultSuggestion(this.defaultSuggestionDescriptions)
        })
    }

    /**
     * Try to search by following a specific query event,
     * and then fallback to default global search if no event matched.
     *
     * While default search is activated, more search work may required.
     * @param query
     * @returns
     */
    async doSearch(query: string): Promise<SearchResult[]> {
        let result: SearchResult[]

        let matchedEvent = this
            .queryEvents
            .sort((a: QueryEvent, b: QueryEvent) => {
                if (a.param.prefix && b.param.prefix) {
                    return b.param.prefix.length - a.param.prefix.length
                }
                return 0
            })
            .find(event => (event.param.prefix && query.startsWith(event.param.prefix))
                        || (event.param.regex && event.param.regex.test(query)))

        if (matchedEvent) {
            result = await matchedEvent.doSearch(query)
            if (matchedEvent.param.onAppend) {
                result.push(...matchedEvent.param.onAppend(query))
            }
        } else {
            if (!this.globalEvent) {
                return []
            }
            result = await this.globalEvent.doSearch(query)
            let defaultSearchEvents = this
                .queryEvents
                .filter(event => {
                    if (event.param.checkIfIsdefaultSearch) {
                        return event.param.checkIfIsdefaultSearch()
                    }
                    return event.defaultSearch
                })
                .sort((a, b) => a.searchPriority - b.searchPriority)

            let defaultSearchAppendixes: SearchResult[] = []
            defaultSearchEvents.forEach(async event => {
                result.push(...await event.doSearch(query))
                if (event.param.onAppend) {
                    defaultSearchAppendixes.push(... event.param.onAppend(query))
                }
            })

            if (this.globalEvent.param.onAppend) {
                result.push(... this.globalEvent.param.onAppend(query))
            }
            result.push(...defaultSearchAppendixes)
        }
        return result
    }

    addNoCachedQueries(...queries: string[]) {
        queries.forEach(query => this.noCacheQueries.add(query))
    }

    addQueryEvent(event: QueryEvent) {
        this.queryEvents.push(event)
    }

    addPrefixQueryEvent(funcs: QueryEventFuncs, prefix: string) {
        // Deepclone may required
        funcs.prefix = prefix
        this.addQueryEvent(new QueryEvent(funcs))
    }

    addRegexQueryEvent(funcs: QueryEventFuncs, regex: RegExp) {
        // Deepclone may required
        funcs.regex = regex
        this.addQueryEvent(new QueryEvent(funcs))
    }

    /**
     * Open the url according to the disposition rule.
     *
     * Disposition rules:
     * - currentTab: enter (default)
     * - newForegroundTab: alt + enter
     * - newBackgroundTab: meta + enter
     */
    static navigateToUrl(url: string, disposition: chrome.omnibox.OnInputEnteredDisposition) {
        url = url.replace(/\?\d+$/ig, "")
        if (disposition === "currentTab") {
            chrome.tabs.query({ active: true }, tab => {
                if (tab[0].id) {
                    chrome.tabs.update(tab[0].id, { url })
                }
            })
        } else {
            chrome.tabs.create({ url })
        }
    }
}
