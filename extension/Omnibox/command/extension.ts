import { CommandHandler, SearchCache } from "./type";

export default class ExtensionHandler extends CommandHandler {
  private static EXTENSION_BASE_URL: string = "https://downloads.haskell.org/ghc/latest/docs/users_guide/";

  private static TRIGGER_PREFIXES: string[] = [":ext", ":extension", ":lan", ":lang", ":language"];

  public static isExtensionMode(input: string): boolean {
    return this.hasTriggerPrefix(input, ...ExtensionHandler.TRIGGER_PREFIXES);
  }

  handleChange(input: string, cache: SearchCache): chrome.omnibox.SuggestResult[] {
    const query = this.removeExtensionPrefix(input);

    throw new Error("Method not implemented.");
  }
  handleEnter(input: string, cache: SearchCache): string {
    throw new Error("Method not implemented.");
  }

  removeExtensionPrefix(input: string): string {
    return this.removeTriggerPrefix(input, ...ExtensionHandler.TRIGGER_PREFIXES);
  }
}
