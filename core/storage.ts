import { StorageItem } from "./types"

/**
 * Mimic localStorage API with chrome.storage.
 * 
 * See also: https://developer.chrome.com/docs/extensions/reference/storage/
 */
export class ExtersionStorage { // TODO: Replace with namespace
    /**
     * Keep `any` to make compiler happy.
     */
    static getAllItems(): Promise<any> {
        return new Promise<any>(resolve => {
            chrome.storage.local.get(null, resolve)
        })
    }

    /**
     * Gets one **or** more items from storage.
     */
    static getItem(key: string): Promise<StorageItem[]> {
        return new Promise<StorageItem[]>(resolve => {
            chrome.storage.local.get(key, (result: {[key:string]: StorageItem[]}) => {
                resolve(result[key])
            })
        })
    }

    static setItem(key: string, val: StorageItem[]): Promise<void> {
        return new Promise<void>(resolve => {
            chrome.storage.local.set({
                [key]: val
            }, resolve)
        })
    }

    /**
     * Removes one or more items from storage.
     */
    static removeItem(key: string): Promise<void> {
        return new Promise<void>(resolve => {
            chrome.storage.local.remove(key, resolve)
        })
    }

    static async migrateLocalStorage(key: string): Promise<void> {
        let val = localStorage.getItem(key)
        let parsed: StorageItem[] | undefined = undefined
        if (val) {
            try {
                parsed = JSON.parse(val)
            } catch (e) {
                console.error(`can't parse ${val} as JSON: ${e}`)
            }
            if (parsed) {
                await this.setItem(key, parsed)
            }
        }
    }
}
