import { linkRawData } from "./linkRawData";

export class LinkData {
  name: string;
  url: string;
  description: string;

  constructor(name: string, url: string, description: string) {
    this.name = name;
    this.url = url;
    this.description = description;
  }
}

export const linkData: LinkData[] = linkRawData.map((x) => new LinkData(x[0], x[1], x[2]));
