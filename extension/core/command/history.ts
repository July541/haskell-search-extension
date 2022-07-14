import { ExtersionStorage } from "../storage"
import { OmniboxContent } from "../types"
import Command from "./base"

export class HistoryCommand extends Command {
    constructor() {
        super("history", "Show your local search history.")
    }

    async onExecute(arg?: any): Promise<OmniboxContent[]> {
        let history = await ExtersionStorage.getItem("history")
        return history
            .filter(item => !arg || item.query.toLowerCase().indexOf(arg) > -1)
            // Make the high frequency appear in front
            .sort((a, b) => b.time - a.time)
            .map(item => {
                return {
                    content: item.content,
                    description: `${item.query} - ${item.description} - Search ${item.time} times`
                }
            })
    }

    onBlankResult(arg: any): chrome.omnibox.SuggestResult[] {
        return [
            {
                content: "No history",
                description: "No history right now, let's search something!"
            }
        ]
    }

    /**
     * Record the search history and reture the history item.
     * @param {string} query The search keyword.
     * @param {object} result The search result.
     * @param {number} maxSize The max size that should keep the search history in local storage.
     * @returns the historyItem.
     */
    static async recordHistory(query?: string, result?: OmniboxContent, maxSize?: number) {
        if (!query || !result) {
            return
        }

        let { content, description } = result
        description = Compat.eliminateTags(description)
        let history = await ExtersionStorage.getItem("history") || []
        let historyItem = { query, content, description, time: Date.now() }
        history.push(historyItem)

        if (maxSize && maxSize >= 0) {
            // Limit the search history to the max size.
            history.sort((a, b) => b.time - a.time).splice(maxSize)
        }

        await ExtersionStorage.setItem("history", history)
        return historyItem
    }
}
