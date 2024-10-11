import { Command, CommandHandler, SearchCache } from "./command/type";
import UnifyHandler from "./command/unify";
import HoogleHandler from "./command/hoogle";
import ExtensionHandler from "./command/extension";
import PackageHandler from "./command/package";
import MetaHandler from "./command/meta";
import LinkHandler from "./command/link";
import ErrorHandler from "./command/error";

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
      if (handler instanceof MetaHandler) {
        return;
      }

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
      } else if (PackageHandler.isPackageMode(input)) {
        return Command.SearchPackage;
      } else if (LinkHandler.isLinkMode(input)) {
        return Command.SearchLink;
      } else if (ErrorHandler.isErrorMode(input)) {
        return Command.SearchError;
      } else if (MetaHandler.isMetaMode(input)) {
        // Don't forget to make meta mode check at last
        return Command.SearchMeta;
      }
      return Command.SearchDefault;
    };
    const dispatchCommand = (command: Command): CommandHandler => {
      switch (command) {
        case Command.SearchPackage:
          return new PackageHandler();
        case Command.SearchHoogle:
          return new HoogleHandler();
        case Command.SearchExtension:
          return new ExtensionHandler();
        case Command.SearchDefault:
          return new UnifyHandler();
        case Command.SearchLink:
          return new LinkHandler();
        case Command.SearchError:
          return new ErrorHandler();
        case Command.SearchMeta:
          return new MetaHandler();
      }
    };

    const command = inferCommand(input);
    return dispatchCommand(command);
  }
}
