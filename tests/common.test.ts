import UnifyHandler from "../extension/omnibox/command/unify";

describe("common", () => {
  it("pager default", () => {
    const handler = new UnifyHandler();
    expect(handler.parsePage("")).toStrictEqual([0, ""]);
  });

  it("pager 0", () => {
    const handler = new UnifyHandler();
    expect(handler.parsePage("arr")).toEqual([0, "arr"]);
  });

  it("pager 1", () => {
    const handler = new UnifyHandler();
    expect(handler.parsePage("arr -")).toEqual([1, "arr"]);
  });

  it("pager 2", () => {
    const handler = new UnifyHandler();
    expect(handler.parsePage("arr --")).toEqual([2, "arr"]);
  });

  it("fake pager", () => {
    const handler = new UnifyHandler();
    expect(handler.parsePage("arr-1")).toEqual([0, "arr-1"]);
  });

  it("- in query", () => {
    const handler = new UnifyHandler();
    expect(handler.parsePage("arr-")).toEqual([0, "arr-"]);
  });
});
