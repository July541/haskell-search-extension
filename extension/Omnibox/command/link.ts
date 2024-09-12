import fuzzysort from "fuzzysort";
import { CommandHandler, SearchCache } from "./type";

export default class LinkHandler extends CommandHandler {
  public static TRIGGER_PREFIX: string = ":url";

  public static isLinkMode(input: string): boolean {
    return this.hasTriggerPrefix(input, this.TRIGGER_PREFIX);
  }

  handleChange(input: string, cache: SearchCache): chrome.omnibox.SuggestResult[] {
    throw new Error("Method not implemented.");
  }

  handleEnter(input: string, cache: SearchCache): string {
    throw new Error("Method not implemented.");
  }

  giveSuggestions(input: string): chrome.omnibox.SuggestResult[] {
    throw new Error("Method not implemented.");
  }
}
