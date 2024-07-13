import { type JestConfigWithTsJest } from "ts-jest";

const chrome = require("sinon-chrome");
const config: JestConfigWithTsJest = {
  verbose: true,
  preset: "ts-jest",
  testEnvironment: "node",
  globals: {
    chrome,
  },
  maxWorkers: 1, // Otherwise the test suite may not respect sinon-chrome
};

export default config;
