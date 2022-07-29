import MultiMap from "@github/multimap";
import { ValueItem } from "./types";

export const processCabalData = (datum: string[][]): MultiMap<string, ValueItem> => {
    const mp = new MultiMap<string, ValueItem>()
    datum.forEach(([key, description]) => {
        mp.set(key, {rendered: key, description, url: key, packageName: key })
    })
    return mp
}

export const processHoogleData = (datum: string[][]): MultiMap<string, ValueItem> => {
    const mp = new MultiMap<string, ValueItem>()
    datum.forEach(([key, rendered, description, url, packageName]) => {
        mp.set(key, { rendered, description, url, packageName })
    })
    return mp
}
