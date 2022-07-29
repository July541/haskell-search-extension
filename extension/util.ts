import MultiMap from "@github/multimap";

export const processMultiKeyData = (datum: string[][]): MultiMap<string, string> => {
    const mp = new MultiMap<string, string>()
    datum.forEach(([a, b]) => {
        mp.set(a, b)
    })
    return mp
}
