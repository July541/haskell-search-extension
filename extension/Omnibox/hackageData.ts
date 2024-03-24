export class HackageData {
    name: string;
    description: string;

    constructor(name: string, description: string) {
        this.name = name;
        this.description = description;
    }
};

const hackageRawData: string[][] = [
    ["base", "base description"], ["array", "array description"],
    ["containers", "containers description"], ["bytestring", "bytestring description"],
    ["text", "text description"], ["vector", "vector description"]
];
export const hackageData: HackageData[] =
    hackageRawData.map(([name, description]) => new HackageData(name, description));
