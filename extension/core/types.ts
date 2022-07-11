interface OmniboxContent {
    content: string
    description: string
}

interface QueryWithPage {
    query: string
    page: number
}

interface OmniboxFunctions {
    onSearch(searchContent: string): Promise<SearchResult[]>
    onAppend(param: string): void
    onEmptyNavigate(param: string): void
    beforeNavigate(): void
    afterNavigate(): void
}

interface QueryEventFuncs {
    onSearch: (searchContent: string) => Promise<SearchResult[]>
    onFormat?: (index: number, searchResult: SearchResult, searchContent: string) => SearchResult
    onAppend?: any
    prefix?: string
    regex?: string
    /**
     * A hook to enable search dynamically.
     */
    defaultSearchHook?: any
}

interface SearchResult {
    event: QueryEvent
}