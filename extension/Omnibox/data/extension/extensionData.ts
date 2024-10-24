import { extensionRawData } from "./extensionRawData";

export enum IncludedVersion {
  Haskell98 = "Haskell98",
  Haskell2010 = "Haskell2010",
  GHC2021 = "GHC2021",
  GHC2024 = "GHC2024",
  NotIncluded = "NA",
}

export type Since = string | undefined;

export class ExtensionData {
  name: string;
  since: Since;
  included: IncludedVersion[];
  deprecated: boolean;
  url: string;

  constructor(name: string, since: Since, deprecated: boolean, included: IncludedVersion[], url: string) {
    this.name = name;
    this.since = since;
    this.deprecated = deprecated;
    this.included = included;
    this.url = url;
  }
}

export const extensionData: ExtensionData[] = extensionRawData.map((raw) => {
  return new ExtensionData(
    raw[0],
    raw[1] == "NA" ? undefined : raw[1],
    raw[2] == "Deprecated",
    raw[3].split(", ").map((o) => o as IncludedVersion),
    raw[4]
  );
});
