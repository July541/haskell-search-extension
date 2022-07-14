import { SearchResult } from "./types"

export interface CachedQuery {
    queryInput: string
    queryResult: SearchResult[]
}
