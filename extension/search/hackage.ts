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
        keyword = keyword.toLowerCase()

        let results: { key: string; matchIndex: number }[] = []

        this.indices.forEach((key, _) => {
            if (keyword.length <= key.length) {
                let matchIndex = key.toLowerCase().indexOf(keyword)
                if (matchIndex !== -1) {
                    results.push({
                        key,
                        matchIndex
                    })
                }
            }
        })

        return results.sort((a, b) => {
            if (a.matchIndex === b.matchIndex) {
                return a.key.length - b.key.length
            }
            return a.matchIndex - b.matchIndex
        }).map(item => {
            return {
                content: item.key,
                path: item.key,
                description: this.rawData[item.key]
            }
        })
    }
}
