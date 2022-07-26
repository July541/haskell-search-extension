import MultiMap from "@github/multimap"
import { ResultType, SearchResult } from "../../core/types"
import { ValueItem } from "../types"

interface TempResult {
    key: string,
    matchIndex: number,
    resultType: ResultType
}

/**
 * Haskell Search Util to search:
 *   1. Hackage packages
 *   2. Function names
 *   3. Function signatures
 */
export class HackageSearcher {
    private cabalData: MultiMap<string, ValueItem>
    private hoogleFuncData: MultiMap<string, ValueItem>

    constructor(cabalData: MultiMap<string, ValueItem>, hoogleFuncData: MultiMap<string, ValueItem>) {
        this.cabalData = cabalData
        this.hoogleFuncData = hoogleFuncData
    }

    async search(keyword: string): Promise<SearchResult[]> {
        let results: TempResult[] = []
        keyword = keyword.toLowerCase()
        this.searchData(keyword, results, this.cabalData, ResultType.Package)
        this.searchData(keyword, results, this.hoogleFuncData, ResultType.HoogleFunc)
        this.sortResult(results)
        return this.constructResultData(results)
    }

    sortResult = (results: TempResult[]) => {
        results.sort((a, b) => {
            if (a.matchIndex === b.matchIndex) {
                return a.key.length - b.key.length
            }
            return a.matchIndex - b.matchIndex
        })
    }

    constructResultData(results: TempResult[]): SearchResult[] {
        return results.flatMap(result => {
            switch (result.resultType) {
                case ResultType.HoogleFunc: return this.constructHoogleFunc(result.key)
                case ResultType.Package: return this.constructCabal(result.key)
            }
        })
    }

    beautyDescription(s: string): string {
        return s.length === 0 ? "No description avaliable" : s
    }

    constructCabal(key: string): SearchResult[] {
        return [...this.cabalData.get(key)].map(item => {
            return {
                content: item.rendered,
                path: item.url,
                description: this.beautyDescription(item.description),
                resultType: ResultType.Package
            }
        })
    }

    constructHoogleFunc(key: string): SearchResult[] {
        return [...this.hoogleFuncData.get(key)].map(item => {
            return {
                content: item.rendered,
                path: item.url,
                description: this.beautyDescription(item.description),
                resultType: ResultType.HoogleFunc,
                data: item.packageName
            }
        })
    }

    async searchData(keyword: string, results: TempResult[], data: MultiMap<string, ValueItem>, resultType: ResultType): Promise<void> {
        [...data.keys()].forEach((key: string) => {
            if (keyword.length <= key.length) {
                let matchIndex = key.toLowerCase().indexOf(keyword)
                if (matchIndex !== -1) {
                    results.push({
                        key,
                        matchIndex,
                        resultType
                    })
                }
            }
        })
    }
}
