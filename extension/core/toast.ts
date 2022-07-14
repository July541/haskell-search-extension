class Toast {
    private element: HTMLElementTagNameMap[keyof HTMLElementTagNameMap] | null
    private dismissTimeoutId?: number

    constructor(selector: keyof HTMLElementTagNameMap) {
        this.element = document.querySelector(selector)
        this.dismissTimeoutId = undefined

        this.element?.addEventListener("mouseover", () => {
            if (this.dismissTimeoutId) {
                clearTimeout(this.dismissTimeoutId)
            }
        })

        this.element?.addEventListener("mouseleave", () => {
            this.dismiss()
        })
    }

    private dismiss(delay: number = 2000) {
        this.dismissTimeoutId = setTimeout(() => this.element!.style.display = "none", delay)
    }

    public info(message: string) {
        if (this.element) {
            this.element.style.display = "block"
            this.element.style.background = "#fbeca0dd"
            this.element.textContent = message
        } else {
            console.error("Unexpected null element")
        }
    }

    public success(message: string) {
        if (this.element) {
            this.element.style.display = "block"
            this.element.style.background = "#357911dd"
            this.element.textContent = message
        } else {
            console.error("Unexpected null element")
        }
    }

    public error(message: string) {
        if (this.element) {
            this.element.style.display = "block"
            this.element.style.background = "#ff0000dd"
            this.element.textContent = message
        } else {
            console.error("Unexpected null element")
        }
    }
}
