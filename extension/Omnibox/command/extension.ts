import fuzzysort from "fuzzysort";
import { ExtensionData, extensionData } from "../data/extension/extensionData";
import { CommandHandler, SearchCache } from "./type";

export default class ExtensionHandler extends CommandHandler {
  private static TRIGGER_PREFIXES: string[] = [":ext", ":extension", ":lan", ":lang", ":language"];

  public static isExtensionMode(input: string): boolean {
    return this.hasTriggerPrefix(input, ...ExtensionHandler.TRIGGER_PREFIXES);
  }

  handleChange(input: string, cache: SearchCache): chrome.omnibox.SuggestResult[] {
    const query = this.removeExtensionPrefix(input);
    const page = this.parsePage(input);
    const startCount = page * this.PAGE_SIZE;
    const endCount = startCount + this.PAGE_SIZE;

    const suggestExtData: ExtensionData[] = fuzzysort
      .go(query, extensionData, { key: "name" })
      .map((x) => x.obj)
      .slice(startCount, endCount);

    const suggestions = suggestExtData.map((x: ExtensionData) => ({
      content: x.name,
      description: `[extension] ${x.name} `,
    }));

    return suggestions;
  }
  handleEnter(input: string, cache: SearchCache): string {
    throw new Error("Method not implemented.");
  }

  removeExtensionPrefix(input: string): string {
    return this.removeTriggerPrefix(input, ...ExtensionHandler.TRIGGER_PREFIXES);
  }
}
