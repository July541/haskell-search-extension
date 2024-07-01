import { Command, CommandHandler, SearchCache } from "./command/type";
import PackageHandler from "./command/package";
import HoogleHandler from "./command/hoogle";
import ExtensionHandler from "./command/extension";

export class Omnibox {
  private cache: SearchCache = new SearchCache();

  bootstrap() {
    chrome.omnibox.onInputChanged.addListener((input: string, suggest) => {
      this.cache.currentInput = input;

      const handler = this.inferHandler(input);
      const suggestions = handler.handleChange(input, this.cache);

      suggest(suggestions);
    });

    chrome.omnibox.onInputEntered.addListener((input: string) => {
      const handler = this.inferHandler(input);
      const url = handler.handleEnter(input, this.cache);
      chrome.tabs.update({ url });
    });
  }

  inferHandler(input: string): CommandHandler {
    const inferCommand = (input: string): Command => {
      if (HoogleHandler.isHoogleMode(input)) {
        return Command.SearchHoogle;
      } else if (ExtensionHandler.isExtensionMode(input)) {
        return Command.SearchExtension;
      }
      return Command.SearchPackage;
    };
    const dispatchCommand = (command: Command): CommandHandler => {
      switch (command) {
        case Command.SearchPackage:
          return new PackageHandler();
        case Command.SearchHoogle:
          return new HoogleHandler();
        case Command.SearchExtension:
          return new ExtensionHandler();
      }
    };

    const command = inferCommand(input);
    return dispatchCommand(command);
  }
}
