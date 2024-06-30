import PackageHandler from "../extension/omnibox/command/package";
import { SearchCache } from "../extension/omnibox/command/type";

describe("package", () => {
    it("Basic package searchp", () => {
        const input = "arr";
        const handler = new PackageHandler();
        const suggest = handler.handleChange(input, new SearchCache());
        expect(suggest[0]).toEqual({
            "content": "https://hoogle.haskell.org/?hoogle=arr",
            "description": "Search arr on [hoogle.haskell.org]"
        });
    })
})
