import { SearchResult } from "../../core/types"

/**
 * Haskell Search Util to search:
 *   1. Hackage packages
 *   2. Function names
 *   3. Function signatures
 */
export class HackageSearcher {
    private rawData: any
    private indices: string[]

    constructor(rawData: any) {
        this.rawData = rawData
        this.indices = Object.keys(rawData)
    }

    async search(keyword: string): Promise<SearchResult[]> {
        return this.indices.map(k => {
            return {
                content: k,
                description: this.rawData[k]
            }
        })
    }
}
