import UnifyHandler from "../extension/omnibox/command/unify";

describe("common", () => {
  it("pager default", () => {
    const handler = new UnifyHandler();
    expect(handler.parsePage("")).toBe(0);
  });

  it("pager 0", () => {
    const handler = new UnifyHandler();
    expect(handler.parsePage("arr")).toBe(0);
  });

  it("pager 1", () => {
    const handler = new UnifyHandler();
    expect(handler.parsePage("arr-")).toBe(1);
  });

  it("pager 2", () => {
    const handler = new UnifyHandler();
    expect(handler.parsePage("arr--")).toBe(2);
  });

  it("fake pager", () => {
    const handler = new UnifyHandler();
    expect(handler.parsePage("arr-1")).toBe(0);
  });
});
