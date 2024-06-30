export enum ExtensionSetVersion {
  Haskell98 = "Haskell98",
  Haskell2010 = "Haskell2010",
  GHC2021 = "GHC2021",
  GHC2024 = "GHC2024",
}

enum DeprecatedStatus {
  Deprecated = "Deprecated",
}

export class ExtensionData {
  name: string;
  /**
   * Since version
   */
  version: string;
  setVersion: ExtensionSetVersion;
  deprecated: boolean;
  urlSuffix: string;

  constructor(name: string, version: string, setVersion: ExtensionSetVersion, urlSuffix: string, deprecated: boolean) {
    this.name = name;
    this.version = version;
    this.setVersion = setVersion;
    this.urlSuffix = urlSuffix;
    this.deprecated = deprecated;
  }
}

const rawData = [
  ["AllowAmbiguousTypes", "7.8.1"],
  ["ApplicativeDo", "8.0.1"],
  ["Arrows", "6.8.1"],
  ["BangPatterns", "6.8.1", ExtensionSetVersion.GHC2021],
  ["BinaryLiterals", "7.10.1", ExtensionSetVersion.GHC2021],
  ["BlockArguments", "8.6.1"],
  ["CApiFFI", "7.6.1"],
  ["ConstrainedClassMethods", "6.8.1"],
  ["ConstraintKinds", "7.4.1", ExtensionSetVersion.GHC2021],
  ["CPP", "6.8.1"],
  ["CUSKs", "8.10.1"],
  ["DataKinds", "7.4.1", ExtensionSetVersion.GHC2024],
  ["DatatypeContexts", "6.8.1"],
  ["DeepSubsumption", "9.2.4"],
  ["DefaultSignatures", "7.2.1"],
  ["DeriveAnyClass", "7.10.1"],
  ["DeriveDataTypeable", "6.8.1", ExtensionSetVersion.GHC2021],
  ["DeriveFoldable", "7.10.1", ExtensionSetVersion.GHC2021],
  ["DeriveFunctor", "7.10.1", ExtensionSetVersion.GHC2021],
  ["DeriveGeneric", "7.2.1", ExtensionSetVersion.GHC2021],
  ["DeriveLift", "8.0.1", ExtensionSetVersion.GHC2021],
  ["DeriveTraversable", "7.10.1", ExtensionSetVersion.GHC2021],
  ["DerivingStrategies", "8.2.1", ExtensionSetVersion.GHC2024],
  ["DerivingVia", "8.6.1"],
  ["DisambiguateRecordFields", "6.8.1", ExtensionSetVersion.GHC2024],
  ["DuplicateRecordFields", "8.0.1"],
  ["EmptyCase", "7.8.1", ExtensionSetVersion.GHC2021],
  ["EmptyDataDecls", "6.8.1", ExtensionSetVersion.Haskell2010],
  ["EmptyDataDeriving", "8.4.1", ExtensionSetVersion.Haskell2010],
  ["ExistentialQuantification", "6.8.1", ExtensionSetVersion.GHC2021],
  ["ExplicitForAll", "6.12.1", ExtensionSetVersion.GHC2021],
  ["ExplicitNamespaces", "7.6.1", ExtensionSetVersion.GHC2024],
];

export const extensionData: ExtensionData[] = rawData.map((raw) => {
  return new ExtensionData(raw[0], raw[1], raw[2] as ExtensionSetVersion, raw[3], raw.length === 5);
});

// [deprecated] {- LANGUAGE OverloadedStrings #-} Since 8.0.1, default starting GHC2021
