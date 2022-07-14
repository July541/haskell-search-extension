import { Omnibox } from "../core/omnibox";

describe("Omnibox", () => {
    describe(".parse()", () => {
        let omnibox = new Omnibox("");
        let inputs = [
            ["std::FILE", {query: "std::FILE", page: 1}],
            ["std::File", {query: "std::File", page: 1}],
            ["cfg", {query: "cfg", page: 1}],
            [" cfg ", {query: "cfg", page: 1}],
            ["!actix", {query: "!actix", page: 1}],
            ["!actix -", {query: "!actix", page: 2}],
            ["!actix --", {query: "!actix", page: 3}],
            ["operator- --", {query: "operator-", page: 3}],
            ["operator new --", {query: "operator new", page: 3}],
            [":book rust", {query: ":book rust", page: 1}],
            [":book  rust", {query: ":book rust", page: 1}],
            [":book rust ", {query: ":book rust", page: 1}],
            [":book rust - ", {query: ":book rust", page: 2}],
            [":book rust -xx ", {query: ":book rust", page: 2}],
            [":book rust -xx- ", {query: ":book rust", page: 3}],
        ];
        inputs.forEach(([input, result]) => {
            test(`Omnibox parse "${input}"`, function() {
                expect(omnibox.parse(input as string)).toStrictEqual(result);
            });
        });
    });
});
