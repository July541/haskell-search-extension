import PackageHandler from "../extension/omnibox/command/package";
import { testSuggestion0 } from "./util";

describe("package", () => {
  it(":pkg", () => {
    const input = ":pkg";
    expect(PackageHandler.isPackageMode(input)).toBe(true);
  });

  it(":pkg trigger", () => {
    const input = ":pkg arrow";
    expect(PackageHandler.isPackageMode(input)).toBe(true);
  });

  it("Package search with :pkg", () => {
    testSuggestion0(":pkg arr", new PackageHandler(), {
      content: "array",
      description: "[package] <match>arr</match>ay - Mutable and immutable arrays",
    });
  });

  it("fake next page", () => {
    testSuggestion0(":pkg arr-", new PackageHandler(), {
      content: "yarr-image-io",
      description: "[package] y<match>arr-</match>image-io - Image IO for Yarr library",
    });
  });

  it("next page", () => {
    testSuggestion0(":pkg arr -", new PackageHandler(), {
      content: "ContArrow",
      description: "[package] Cont<match>Arr</match>ow - Control.Arrow.Transformer.Cont",
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
