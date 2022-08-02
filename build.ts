import { Builder, Browser } from "./utils/builder"

const watchFlag = process.argv.includes("--watch")
const devFlag = process.argv.includes("--dev")
const chromeFlag = process.argv.includes("--chrome")
const firefoxFlag = process.argv.includes("--firefox")
const edgeFlag = process.argv.includes("--edge")
const browser: Browser =
    chromeFlag ? "chrome"
               : firefoxFlag ? "firefox"
                             : edgeFlag ? "edge" : "chrome"

const builder = new Builder(browser, watchFlag, devFlag)
builder.addBuildFile("./extension/main.ts")
builder.addStaticFile("./logo.png")

builder.build()
