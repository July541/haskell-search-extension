type OnSearchType = (searchContent: string) => Promise<SearchResult[]>

class QueryEvent {
    private param: QueryEventFuncs
    private searchContent: string
    private defaultSearch: boolean
    /**
     * The default search priority. The smaller, the higher.
     */ 
    private searchPriority: number

    constructor(param: QueryEventFuncs, defaultSearch: boolean = false, searchPriority: number = 0) {
        this.param = param
        this.defaultSearch = defaultSearch
        this.searchPriority = searchPriority
    }

    async doSearch(input: string) {
        this.searchContent = input
        let result = await this.param.onSearch(input)
        return result.map(item => {
            item.event = this
            return item
        })
    }

    doFormat(item: SearchResult, index: number): SearchResult {
        if (this.param.onFormat) {
            item = this.param.onFormat(index, item, this.searchContent)
        }
        return item
    }
}