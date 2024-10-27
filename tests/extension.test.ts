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

  it("default behavior", () => {
    testSuggestion0(":ext ", new ExtensionHandler(), {
      content: ":ext AllowAmbiguousTypes",
      description: "{-# LANGUAGE AllowAmbiguousTypes #-} Since 7.8.1",
    });
  });

  it("deprecated extension", () => {
    testSuggestion0(":ext incoher", new ExtensionHandler(), {
      content: ":ext IncoherentInstances",
      description: "[deprecated] {-# LANGUAGE IncoherentInstances #-} Since 6.8.1",
    });
  });

  it("default starting GHC2021", () => {
    testSuggestion0(":ext BangPatterns", new ExtensionHandler(), {
      content: ":ext BangPatterns",
      description: "{-# LANGUAGE BangPatterns #-} Since 6.8.1 Included in GHC2024,GHC2021",
    });
  });
  it("suggestion number", () => {
    const input = ":ext ex";
    const handler = new ExtensionHandler();
    const suggestions = handler.giveSuggestions(input);
    expect(suggestions.length).toBe(10);
  });

  it("no suggestion", () => {
    const input = ":ext xxxxxxxxxx";
    const handler = new ExtensionHandler();
    const suggestions = handler.giveSuggestions(input);
    expect(suggestions.length).toBe(0);
  });

  it("next page", () => {
    testSuggestion0(":ext a -", new ExtensionHandler(), {
      content: ":ext Safe",
      description: "{-# LANGUAGE Safe #-} Since 7.2.1",
    });
  });

  it("Haskell98 included count", () => {
    expect(ExtensionHandler.Haskell98_Included.length).toBe(7);
  });

  it("Haskell2010 included count", () => {
    expect(ExtensionHandler.Haskell2010_Included.length).toBe(9);
  });

  it("GHC2021 included count", () => {
    expect(ExtensionHandler.GHC2021_Included.length).toBe(41);
  });

  it("GHC2024 included count", () => {
    expect(ExtensionHandler.GHC2024_Included.length).toBe(49);
  });
});
