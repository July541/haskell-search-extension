import { hackageRawData } from "./hackageRawData";
export class HackageData {
    name: string
    description: string;

    constructor(name: string, description: string) {
        this.name = name;
        this.description = description;
    }
};

export const hackageData: HackageData[] =
    hackageRawData.map(([name, description]) => new HackageData(name, description));
