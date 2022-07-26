import { SearchResult, QueryEventFuncs } from "./types"

type OnSearchType = (searchContent: string) => Promise<SearchResult[]>

export class QueryEvent {
    public param: QueryEventFuncs
    private searchContent: string
    public defaultSearch: boolean
    /**
     * The default search priority. The smaller, the higher.
     */
    public searchPriority: number

    constructor(param: QueryEventFuncs, defaultSearch: boolean = false, searchPriority: number = 0) {
        this.param = param
        this.defaultSearch = defaultSearch
        this.searchPriority = searchPriority
        this.searchContent = ""
    }

    async doSearch(input: string) {
        this.searchContent = input
        let result = await this.param.onSearch(input)
        return result.map(item => {
            item.event = this
            return item
        })
    }

    doFormat(content: SearchResult, index: number): SearchResult {
        return this.param.onFormat(index, content)
    }
}
