import path from "path"
import { Jsonnet } from "@hanazuki/node-jsonnet";
import * as fs from "fs/promises"
import { build, BuildOptions } from "esbuild"
import * as chokidar from "chokidar"

export type Browser = "chrome" | "firefox" | "edge"
const defaultManifestFileName = "manifest.json"
const defaultJsonnetFilePath = "./manifest.jsonnet"

const distDir = (browser: Browser): string => {
    switch (browser) {
        case "chrome": return "dist-firefox"
        case "firefox": return "dist-firefox"
        case "edge": return "dist-edge"
    }
}

const distFullPath = (relativePath: string, targetBrowser: Browser): string =>
    path.join(distDir(targetBrowser), relativePath)

const makeManifestFile = async (jsonnetFilePath: string, browser: Browser): Promise<void> => {
    const jsonnet = new Jsonnet()
    const result: string = await jsonnet.evaluateFile(jsonnetFilePath)
    fs.writeFile(distFullPath(defaultManifestFileName, browser), result)
}

export class Builder {
    private watchFlag: boolean
    private devFlag: boolean
    private browserType: Browser
    private buildFiles: string[]
    private staticFiles: string[]
    private staticDirs: string []

    constructor(browser: Browser, watchFlag: boolean, devFlag: boolean) {
        this.watchFlag = watchFlag
        this.devFlag = devFlag
        this.browserType = browser
        this.buildFiles = []
        this.staticFiles = []
        this.staticDirs = []
    }

    addBuildFile(file: string) {
        this.buildFiles.push(file)
    }

    addStaticFile(file: string) {
        this.staticFiles.push(file)
    }

    addStaticDir(file: string) {
        this.staticDirs.push(file)
    }

    watchOption(browser: Browser): BuildOptions["watch"] {
        return this.watchFlag ?
            {
                onRebuild: (error, result) => {
                    if (error) {
                        console.error(`Watch build failed for ${browser}: `, error)
                    }
                    if (result) { // error may have warnings, so we prevent to use `else` here
                        console.log(`watch build succeeded for ${browser}:`, result)
                    }
                }
            }
            : false
    }

    async copyStaticFile(file: string, browser: Browser): Promise<void> {
        await fs.mkdir(distFullPath(path.dirname(file), browser), { recursive: true })

        const cp = (file: string) => fs.copyFile(file, distFullPath(file, browser))

        if (this.watchFlag) {
            chokidar
                .watch(file)
                .on("all", (event, path) => {
                    console.log(event, path)
                    cp(path)
                })
        } else {
            cp(file)
        }
    }

    async copyStaticDir(dir: string, browser: Browser): Promise<void> {
        await fs.mkdir(distFullPath(dir, browser), { recursive: true })
        
        if (this.watchFlag) {
            chokidar
                .watch(path.join(dir, "*"))
                .on("all", (event, filepath) => {
                    console.log(event, filepath)
                    fs.copyFile(
                        filepath,
                        distFullPath(path.join(dir, path.basename(filepath)), browser)
                    )
                })
        } else {
            fs.cp(dir, distFullPath(dir, browser), { recursive: true })
        }
    }

    makeManifestFileAndWatch(jsonnetFilePath: string, browser: Browser): void {
        const mk = () => makeManifestFile(jsonnetFilePath, browser)

        if (this.watchFlag) {
            chokidar
                .watch(defaultManifestFileName)
                .on("all", (event, path) => {
                    console.log(event, path)
                    mk()
                })
        } else {
            mk()
        }
    }

    build(jsonnetFilePath: string = defaultJsonnetFilePath) {
        this.buildFiles.forEach(file => {
            build({
                entryPoints: [file],
                bundle: true,
                outdir: distFullPath(path.dirname(file), this.browserType),
                watch: this.watchOption(this.browserType),
                sourcemap: this.devFlag ? "inline" : false
            })
        })

        this.staticFiles.forEach(file => {
            this.copyStaticFile(file, this.browserType)
        })

        this.staticDirs.forEach(dir => {
            this.copyStaticDir(dir, this.browserType)
        })

        this.makeManifestFileAndWatch(jsonnetFilePath, this.browserType)
    }
}
