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

export const linkData: LinkData[] = [
  ["Haskell", "https://www.haskell.org/", "Haskell language official page"],
  ["GHC", "https://www.haskell.org/ghc/", "The Glasgow Haskell Compiler"],
  ["Hackage", "https://hackage.haskell.org/", "The Haskell Package Repository"],
  ["Stackage", "https://www.stackage.org/", "Stable Haskell package sets"],
  ["Discourse", "https://discourse.haskell.org/", "Haskell community forum"],
  ["GHCup", "https://www.haskell.org/ghcup/", "The Haskell toolchain installer"],
  ["HLS", "https://haskell-language-server.readthedocs.io/en/latest/", "Haskell Language Server docs"],
  ["Cabal", "https://cabal.readthedocs.io/en/stable/", "Building and packaging Haskell libraries and programs"],
  ["Stack", "https://docs.haskellstack.org/en/stable/", "A program for developing Haskell projects"],
  ["Wiki", "https://wiki.haskell.org/", "Haskell Wiki"],
  [
    "Unfolder",
    "https://www.youtube.com/watch?v=S_HSt6jEtWM&list=PLD8gywOEY4HaG5VSrKVnHxCptlJv2GAn7",
    "The Haskell Unfolder Episode",
  ],
  ["Blog", "https://blog.haskell.org/", "The Haskell Programming Language's blog"],
].map((x) => new LinkData(x[0], x[1], x[2]));
