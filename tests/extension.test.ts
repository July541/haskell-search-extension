import ExtensionHandler from "../extension/omnibox/command/extension";
import { testSuggestion, testSuggestion0 } from "./util";

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

  it("expandSuggestionForIncluded Haskell98", () => {
    testSuggestion(":ext Haskell98", new ExtensionHandler(), 2, {
      content: ":ext DatatypeContexts",
      description: "[deprecated] {-# LANGUAGE DatatypeContexts #-} Since 7.0.1 Included in Haskell98,Haskell2010",
    });
  });

  it("expandSuggestionForIncluded Haskell2010", () => {
    testSuggestion(":ext Haskell2010", new ExtensionHandler(), 3, {
      content: ":ext EmptyDataDecls",
      description: "{-# LANGUAGE EmptyDataDecls #-} Since 6.8.1 Included in GHC2024,GHC2021,Haskell2010",
    });
  });

  it("expandSuggestionForIncluded GHC2021", () => {
    testSuggestion(":ext GHC2021", new ExtensionHandler(), 3, {
      content: ":ext ConstrainedClassMethods",
      description: "{-# LANGUAGE ConstrainedClassMethods #-} Since 6.8.1 Included in GHC2024,GHC2021",
    });
  });

  it("expandSuggestionForIncluded GHC2021 next page", () => {
    testSuggestion0(":ext GHC2021 -", new ExtensionHandler(), {
      content: ":ext DeriveTraversable",
      description: "{-# LANGUAGE DeriveTraversable #-} Since 7.10.1 Included in GHC2024,GHC2021",
    });
  });

  it("expandSuggestionForIncluded GHC2021 next two page", () => {
    testSuggestion0(":ext GHC2021 --", new ExtensionHandler(), {
      content: ":ext GADTSyntax",
      description: "{-# LANGUAGE GADTSyntax #-} Since 7.2.1 Included in GHC2024,GHC2021",
    });
  });

  it("expandSuggestionForIncluded GHC2024", () => {
    testSuggestion(":ext GHC2024", new ExtensionHandler(), 2, {
      content: ":ext BinaryLiterals",
      description: "{-# LANGUAGE BinaryLiterals #-} Since 7.10.1 Included in GHC2024,GHC2021",
    });
  });

  it("expandSuggestionForIncluded GHC2024 next page", () => {
    testSuggestion0(":ext GHC2024 -", new ExtensionHandler(), {
      content: ":ext DeriveLift",
      description: "{-# LANGUAGE DeriveLift #-} Since 8.0.1 Included in GHC2024,GHC2021",
    });
  });

  it("expandSuggestionForIncluded GHC2024 next two page", () => {
    testSuggestion0(":ext GHC2024 --", new ExtensionHandler(), {
      content: ":ext FieldSelectors",
      description: "{-# LANGUAGE FieldSelectors #-} Since 9.2.1 Included in GHC2024,GHC2021,Haskell2010,Haskell98",
    });
  });
});
