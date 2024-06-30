export enum ExtensionSetVersion {
  Haskell98 = "Haskell98",
  Haskell2010 = "Haskell2010",
  GHC2021 = "GHC2021",
  GHC2024 = "GHC2024",
  EXPLICIT_IMPORT = "EXPLICIT_IMPORT",
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
  [
    "AllowAmbiguousTypes",
    "7.8.1",
    ExtensionSetVersion.EXPLICIT_IMPORT,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/ambiguous_types.html#extension-AllowAmbiguousTypes",
  ],
  [
    "ApplicativeDo",
    "8.0.1",
    ExtensionSetVersion.EXPLICIT_IMPORT,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/applicative_do.html#extension-ApplicativeDo",
  ],
  [
    "Arrows",
    "6.8.1",
    ExtensionSetVersion.EXPLICIT_IMPORT,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/arrows.html#extension-Arrows",
  ],
  [
    "BangPatterns",
    "6.8.1",
    ExtensionSetVersion.GHC2021,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/strict.html#extension-BangPatterns",
  ],
  [
    "BinaryLiterals",
    "7.10.1",
    ExtensionSetVersion.GHC2021,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/binary_literals.html#extension-BinaryLiterals",
  ],
  [
    "BlockArguments",
    "8.6.1",
    ExtensionSetVersion.EXPLICIT_IMPORT,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/block_arguments.html#extension-BlockArguments",
  ],
  [
    "CApiFFI",
    "7.6.1",
    ExtensionSetVersion.EXPLICIT_IMPORT,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/ffi.html#extension-CApiFFI",
  ],
  [
    "ConstrainedClassMethods",
    "6.8.1",
    ExtensionSetVersion.EXPLICIT_IMPORT,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/constrained_class_methods.html#extension-ConstrainedClassMethods",
  ],
  [
    "ConstraintKinds",
    "7.4.1",
    ExtensionSetVersion.GHC2021,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/constraint_kind.html#extension-ConstraintKinds",
  ],
  [
    "CPP",
    "6.8.1",
    ExtensionSetVersion.EXPLICIT_IMPORT,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/phases.html#extension-CPP",
  ],
  [
    "CUSKs",
    "8.10.1",
    ExtensionSetVersion.EXPLICIT_IMPORT,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/poly_kinds.html#extension-CUSKs",
    DeprecatedStatus.Deprecated,
  ],
  [
    "DataKinds",
    "7.4.1",
    ExtensionSetVersion.GHC2024,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/data_kinds.html#extension-DataKinds",
  ],
  [
    "DatatypeContexts",
    "6.8.1",
    ExtensionSetVersion.EXPLICIT_IMPORT,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/datatype_contexts.html#extension-DatatypeContexts",
  ],
  [
    "DeepSubsumption",
    "9.2.4",
    ExtensionSetVersion.EXPLICIT_IMPORT,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/rank_polymorphism.html#extension-DeepSubsumption",
  ],
  [
    "DefaultSignatures",
    "7.2.1",
    ExtensionSetVersion.EXPLICIT_IMPORT,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/default_signatures.html#extension-DefaultSignatures",
  ],
  [
    "DeriveAnyClass",
    "7.10.1",
    ExtensionSetVersion.EXPLICIT_IMPORT,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/derive_any_class.html#extension-DeriveAnyClass",
  ],
  [
    "DeriveDataTypeable",
    "6.8.1",
    ExtensionSetVersion.GHC2021,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/deriving_extra.html#extension-DeriveDataTypeable",
  ],
  [
    "DeriveFoldable",
    "7.10.1",
    ExtensionSetVersion.GHC2021,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/deriving_extra.html#extension-DeriveFoldable",
  ],
  [
    "DeriveFunctor",
    "7.10.1",
    ExtensionSetVersion.GHC2021,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/deriving_extra.html#extension-DeriveFunctor",
  ],
  [
    "DeriveGeneric",
    "7.2.1",
    ExtensionSetVersion.GHC2021,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/generics.html#extension-DeriveGeneric",
  ],
  [
    "DeriveLift",
    "8.0.1",
    ExtensionSetVersion.GHC2021,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/deriving_extra.html#extension-DeriveLift",
  ],
  [
    "DeriveTraversable",
    "7.10.1",
    ExtensionSetVersion.GHC2021,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/deriving_extra.html#extension-DeriveTraversable",
  ],
  [
    "DerivingStrategies",
    "8.2.1",
    ExtensionSetVersion.GHC2024,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/deriving_strategies.html#extension-DerivingStrategies",
  ],
  [
    "DerivingVia",
    "8.6.1",
    ExtensionSetVersion.EXPLICIT_IMPORT,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/deriving_via.html#extension-DerivingVia",
  ],
  [
    "DisambiguateRecordFields",
    "6.8.1",
    ExtensionSetVersion.GHC2024,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/disambiguate_record_fields.html#extension-DisambiguateRecordFields",
  ],
  [
    "DuplicateRecordFields",
    "8.0.1",
    ExtensionSetVersion.EXPLICIT_IMPORT,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/duplicate_record_fields.html#extension-DuplicateRecordFields",
  ],
  [
    "EmptyCase",
    "7.8.1",
    ExtensionSetVersion.GHC2021,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/empty_case.html#extension-EmptyCase",
  ],
  [
    "EmptyDataDecls",
    "6.8.1",
    ExtensionSetVersion.Haskell2010,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/nullary_types.html#extension-EmptyDataDecls",
  ],
  [
    "EmptyDataDeriving",
    "8.4.1",
    ExtensionSetVersion.Haskell2010,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/empty_data_deriving.html#extension-EmptyDataDeriving",
  ],
  [
    "ExistentialQuantification",
    "6.8.1",
    ExtensionSetVersion.GHC2021,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/existential_quantification.html#extension-ExistentialQuantification",
  ],
  [
    "ExplicitForAll",
    "6.12.1",
    ExtensionSetVersion.GHC2021,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/explicit_forall.html#extension-ExplicitForAll",
  ],
  [
    "ExplicitNamespaces",
    "7.6.1",
    ExtensionSetVersion.GHC2024,
    "https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/explicit_namespaces.html#extension-ExplicitNamespaces",
  ],
];

export const extensionData: ExtensionData[] = rawData.map((raw) => {
  return new ExtensionData(raw[0], raw[1], raw[2] as ExtensionSetVersion, raw[3], raw.length === 5);
});

// [deprecated] {- LANGUAGE OverloadedStrings #-} Since 8.0.1, default starting GHC2021
