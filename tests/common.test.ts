import DefaultHandler from "../extension/omnibox/command/default";
import { pagerEqual } from "./util";

describe("common", () => {
  it("pager default", () => {
    pagerEqual(new DefaultHandler(), "", 0, "");
  });

  it("pager 0", () => {
    pagerEqual(new DefaultHandler(), "arr", 0, "arr");
  });

  it("pager 1", () => {
    pagerEqual(new DefaultHandler(), "arr -", 1, "arr");
  });

  it("pager 2", () => {
    pagerEqual(new DefaultHandler(), "arr --", 2, "arr");
  });

  it("fake pager", () => {
    pagerEqual(new DefaultHandler(), "arr-1", 0, "arr-1");
  });

  it("- in query", () => {
    pagerEqual(new DefaultHandler(), "arr-", 0, "arr-");
  });
});
