export class Compat {
    public static browserType(): "firefox" | "edge" | "chrome" | "unknown" {
        let userAgent = navigator.userAgent.toLowerCase()

        if (userAgent.indexOf("firefox") !== -1)
            return "firefox"
        if (userAgent.indexOf("edg") !== -1)
            return "edge"
        if (userAgent.indexOf("chrome") !== -1)
            return "chrome"

        return "unknown"
    }

    public static omniboxPageSize(): number {
        return { "firefox": 6,
                 "edge": 7,
                 "chrome": 8,
                 "unknown": 6
               } [this.browserType()]
    }

    /**
     * Escape the five predefined entities to display them as text.
     * @param str
     */
    public static escape(str?: string): string {
        str = str || ""
        return str
        // return this.browserType() === "firefox"
        //      ? str
        //      : str.replace(/&/g, "&amp;")
        //           .replace(/</g, "&lt;")
        //           .replace(/>/g, "&gt;")
        //           .replace(/"/g, "&quot;")
        //           .replace(/'/g, "&#039;")
    }

    public static normalizedDateString(date: Date): string {
        let month = (date.getMonth() + 1).toString() // FIXME: why plus one here?
        let day = date.getDate().toString()
        let year = date.getFullYear().toString()
        return [year, month.padStart(2, "0"), day.padStart(2, "0")].join("-")
    }

    public static capitalize(s?: string): string {
        if (!s || s.length === 0) {
            return ""
        }
        return s.charAt(0).toUpperCase() + s.slice(1)
    }

    public static eliminateTags(value?: string): string {
        if (!value) {
            return ""
        }
        return value.replace(/<\/?(match|dim|code|em|strong)>/g, "")
    }

    public static tagged(tag: string, s: string): string {
        if (this.browserType() !== "firefox") {
            return `<${tag}>${s}</${tag}>`
        }
        return s
    }

    public static taggedMatch(s: string): string {
        return this.tagged("match", s)
    }

    public static taggedDim(s: string): string {
        return this.tagged("dim", s)
    }
}
