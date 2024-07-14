import PackageHandler from "../extension/omnibox/command/package";
import { testSuggestion0 } from "./util";

describe("package", () => {
  it(":pkg trigger", () => {
    const input = ":pkg arrow";
    expect(PackageHandler.isPackageMode(input)).toBe(true);
  });

  it(":package trigger", () => {
    const input = ":package arrow";
    expect(PackageHandler.isPackageMode(input)).toBe(true);
  });

  it("not trigger", () => {
    const input = ":pack arrow";
    expect(PackageHandler.isPackageMode(input)).toBe(false);
  });

  it("Package search with :pkg", () => {
    testSuggestion0(":pkg arr", new PackageHandler(), {
      content: "array",
      description: "[package] array - Mutable and immutable arrays",
    });
  });

  it("Package search with :package", () => {
    testSuggestion0(":package arr", new PackageHandler(), {
      content: "array",
      description: "[package] array - Mutable and immutable arrays",
    });
  });

  it("next page", () => {
    testSuggestion0(":pkg arr-", new PackageHandler(), {
      content: "array-memoize",
      description: "[package] array-memoize - Memoization combinators using arrays for finite sub-domains of functions",
    });
  });

  // I don't want to perform the following tests because of https://stackoverflow.com/questions/78744243/jest-has-different-behavior-in-watch-mode

  // it("Package search default value", () => {
  //   testHandleEnterWithDefaultInput(":pkg arr", new PackageHandler(), "https://hackage.haskell.org/package/array");
  // });

  // it("Package search with selection", () => {
  //   testHandleEnterWithSelection(
  //     ":pkg arr",
  //     "arrows",
  //     new PackageHandler(),
  //     "https://hackage.haskell.org/package/arrows"
  //   );
  // });
});
