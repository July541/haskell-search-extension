import UnifyHandler from "../extension/omnibox/command/unify";
import { pagerEqual } from "./util";

describe("common", () => {
  it("pager default", () => {
    pagerEqual(new UnifyHandler(), "", 0, "");
  });

  it("pager 0", () => {
    pagerEqual(new UnifyHandler(), "arr", 0, "arr");
  });

  it("pager 1", () => {
    pagerEqual(new UnifyHandler(), "arr -", 1, "arr");
  });

  it("pager 2", () => {
    pagerEqual(new UnifyHandler(), "arr --", 2, "arr");
  });

  it("fake pager", () => {
    pagerEqual(new UnifyHandler(), "arr-1", 0, "arr-1");
  });

  it("- in query", () => {
    pagerEqual(new UnifyHandler(), "arr-", 0, "arr-");
  });
});
