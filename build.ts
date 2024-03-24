import * as esbuild from 'esbuild';
import * as fs from "fs/promises";

type Browser = 'chrome' | 'firefox' | 'edge';

class Builder {
    private watch: boolean;
    private dev: boolean;
    private browser: Browser;
    private buildFile: string;
    private staticFiles: File[];
    private staticDirs: string[];

    constructor(browser: Browser, watch: boolean, dev: boolean, buildFile: string) {
        this.browser = browser;
        this.watch = watch;
        this.dev = dev;
        this.buildFile = buildFile;
        this.staticFiles = [];
        this.staticDirs = [];
    }

    getOutDir(): string {
        return `./dist-${this.browser}`;
    }

    build() {
        const buildOptions: esbuild.BuildOptions = {
            entryPoints: [this.buildFile],
            bundle: true,
            outdir: this.getOutDir()
        };
        esbuild.build(buildOptions);
        fs.copyFile('./manifest.json', `${this.getOutDir()}/manifest.json`);
    }
}

const watchFlag = process.argv.includes('--watch');
const devFlag = process.argv.includes('--dev');
const chromeFlag = process.argv.includes('--chrome');
const firefoxFlag = process.argv.includes('--firefox');
const edgeFlag = process.argv.includes('--edge');

const browser: Browser = chromeFlag ? 'chrome' : firefoxFlag ? 'firefox' : edgeFlag ? 'edge' : 'chrome';
const builder = new Builder(browser, watchFlag, devFlag, "./extension/main.ts");

builder.build()
