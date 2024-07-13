import PackageHandler from "../extension/omnibox/command/package";
import { CommandHandler, SearchCache } from "../extension/omnibox/command/type";
import { testHandleChange, testHandleEnterWithDefaultInput, testHandleEnterWithSelection } from "./util";

describe("package", () => {
  it("Package search with :pkg", () => {
    testHandleChange(":pkg arr", new PackageHandler(), {
      content: "array",
      description: "[package] array - Mutable and immutable arrays",
    });
  });

  it("Package search with :package", () => {
    testHandleChange(":package arr", new PackageHandler(), {
      content: "array",
      description: "[package] array - Mutable and immutable arrays",
    });
  });

  it("Package search default value", () => {
    testHandleEnterWithDefaultInput(":pkg arr", new PackageHandler(), "https://hackage.haskell.org/package/array");
  });

  it("Package search with selection", () => {
    testHandleEnterWithSelection(
      ":pkg arr",
      "arrows",
      new PackageHandler(),
      "https://hackage.haskell.org/package/arrows"
    );
  });
});
