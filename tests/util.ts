import { CommandHandler, SearchCache } from "../extension/omnibox/command/type";

/**
 * Test the `handleChange` method if give the expected result.
 *
 * **NOTE: the suggest[0] turns to the second suggestion in the omnibox
 * sice the first is in the default.**
 * @param input The original user input
 * @param handler
 * @param expected
 */
export function testHandleChange(input: string, handler: CommandHandler, expected: chrome.omnibox.SuggestResult) {
  const cache = new SearchCache();
  const suggest = handler.giveSuggestions(input);
  expect(suggest[0]).toEqual(expected);
}

/**
 * Test the `handleEnter` method with default input(the user input)
 * @param input The original user input
 * @param handler
 * @param expected
 */
export function testHandleEnterWithDefaultInput(input: string, handler: CommandHandler, expected: string) {
  const cache = new SearchCache();
  cache.currentInput = input;
  // Calling `handleChange` to make sure the cache is updated.
  const _ = handler.handleChange(input, cache);
  const url = handler.handleEnter(input, cache);
  expect(url).toBe(expected);
}

/**
 * Test the `handleEnter` method with user specific selection,
 * like user input `aaa` and entering `aaaa`.
 * @param input The original user input
 * @param selection The user selected value while entering
 * @param handler
 * @param expected
 */
export function testHandleEnterWithSelection(
  input: string,
  selection: string,
  handler: CommandHandler,
  expected: string
) {
  const cache = new SearchCache();
  cache.currentInput = input;
  // Calling `handleChange` to make sure the cache is updated.
  const _ = handler.handleChange(input, cache);
  const url = handler.handleEnter(selection, cache);
  expect(url).toBe(expected);
}
