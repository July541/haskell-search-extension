import ExtensionHandler from "../extension/omnibox/command/extension";
import { testSuggestion0 } from "./util";

describe("extension", () => {
  it(":ext", () => {
    const input = ":ext";
    expect(ExtensionHandler.isExtensionMode(input)).toBe(true);
  });

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
    testSuggestion0(":ext ", new ExtensionHandler(), {
      content: ":ext AllowAmbiguousTypes",
      description: "{-# LANGUAGE AllowAmbiguousTypes #-} Since 7.8.1",
    });
  });

  it("deprecated extension", () => {
    testSuggestion0(":ext cu", new ExtensionHandler(), {
      content: ":ext CUSKs",
      description: "[deprecated] {-# LANGUAGE CUSKs #-} Since 8.10.1",
    });
  });

  it("default starting GHC2021", () => {
    testSuggestion0(":ext BangPatterns", new ExtensionHandler(), {
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
