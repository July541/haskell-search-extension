/** @type {import('ts-jest').JestConfigWithTsJest} */
const chrome = require('sinon-chrome');
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
  globals: {
    chrome
  }
};
