import { CachedQuery } from "./cache"
import { QueryEvent } from "./query-event"

export type OmniboxContent = chrome.omnibox.SuggestResult

export interface OmniboxParam {
    name: string
    url: string
    description?: string
}

export interface QueryWithPage {
    query: string
    page: number
}

interface BaseFunctions {
    onSearch(searchContent: string): Promise<SearchResult[]>
    onFormat: (index: number, content: SearchResult) => SearchResult
}

export interface OmniboxFunctions extends BaseFunctions {
    onAppend(param: string): void
    onEmptyNavigate?: (content: string, disposition: chrome.omnibox.OnInputEnteredDisposition) => Promise<void>
    beforeNavigate?: (cached?: CachedQuery, content?: string) => Promise<string>
    afterNavigate?: (cached?: CachedQuery, searchResult?: SearchResult) => Promise<void>
}

export interface QueryEventFuncs extends BaseFunctions {
    onAppend?: (query: string) => SearchResult[]
    prefix?: string
    regex?: RegExp
    /**
     * A hook to enable search dynamically.
     */
    checkIfIsdefaultSearch?: () => boolean
}

export interface SearchResult extends chrome.omnibox.SuggestResult {
    event?: QueryEvent
    path?: string
}

export interface StorageItem extends OmniboxContent {
    query: string
    time: number
}
