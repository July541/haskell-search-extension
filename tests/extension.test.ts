import ExtensionHandler from "../extension/omnibox/command/extension";

describe("extension", () => {
  it(":ext trigger", () => {
    const input = ":ext arrow";
    expect(ExtensionHandler.isExtensionMode(input)).toBe(true);
  });
  it(":extension trigger", () => {
    const input = ":extension arrow";
    expect(ExtensionHandler.isExtensionMode(input)).toBe(true);
  });
  it(":lan trigger", () => {
    const input = ":lan arrow";
    expect(ExtensionHandler.isExtensionMode(input)).toBe(true);
  });
  it(":lang trigger", () => {
    const input = ":lang arrow";
    expect(ExtensionHandler.isExtensionMode(input)).toBe(true);
  });
  it(":language trigger", () => {
    const input = ":language arrow";
    expect(ExtensionHandler.isExtensionMode(input)).toBe(true);
  });
  it("default behavior", () => {
    const input = ":ext ";
    const handler = new ExtensionHandler();
    const suggestions = handler.giveSuggestions(input);
    expect(suggestions[0]).toEqual({
      content: ":ext AllowAmbiguousTypes",
      description: "{-# LANGUAGE AllowAmbiguousTypes #-} Since 7.8.1",
    });
  });
  it("deprecated extension", () => {
    const input = ":ext cu";
    const handler = new ExtensionHandler();
    const suggestions = handler.giveSuggestions(input);
    expect(suggestions[0]).toEqual({
      content: ":ext CUSKs",
      description: "[deprecated] {-# LANGUAGE CUSKs #-} Since 8.10.1",
    });
  });
  it("default starting GHC2021", () => {
    const input = ":ext BangPatterns";
    const handler = new ExtensionHandler();
    const suggestions = handler.giveSuggestions(input);
    expect(suggestions[0]).toEqual({
      content: ":ext BangPatterns",
      description: "{-# LANGUAGE BangPatterns #-} Since 6.8.1, default starting GHC2021",
    });
  });
  it("suggestion number", () => {
    const input = ":ext ex";
    const handler = new ExtensionHandler();
    const suggestions = handler.giveSuggestions(input);
    expect(suggestions.length).toBe(4);
  });
  it("no suggestion", () => {
    const input = ":ext xxxxxxxxxx";
    const handler = new ExtensionHandler();
    const suggestions = handler.giveSuggestions(input);
    expect(suggestions.length).toBe(0);
  });
});
