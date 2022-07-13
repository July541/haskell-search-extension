interface OmniboxContent {
    content: string
    description: string
}

interface OmniboxParam {
    name: string
    url: string
    description?: string
}

interface QueryWithPage {
    query: string
    page: number
}

interface OmniboxFunctions {
    onSearch(searchContent: string): Promise<SearchResult[]>
    onAppend(param: string): void
    onEmptyNavigate?: (content: string, disposition: chrome.omnibox.OnInputEnteredDisposition) => Promise<void>
    beforeNavigate?: (cached?: CachedQuery, content?: string) => Promise<string>
    afterNavigate?: (cached?: CachedQuery, searchResult?: SearchResult) => Promise<void>
}

interface QueryEventFuncs {
    onSearch: (searchContent: string) => Promise<SearchResult[]>
    onFormat?: (index: number, content: chrome.omnibox.SuggestResult, searchContent: string) => void
    onAppend?: (query: string) => SearchResult[]
    prefix?: string
    regex?: RegExp
    /**
     * A hook to enable search dynamically.
     */
    checkIfIsdefaultSearch?: () => boolean
}

interface SearchResult extends chrome.omnibox.SuggestResult {
    event?: QueryEvent
}
