{
  new(name, keyword, description, version):: {
    local it = self,
    _icons:: {},
    _background_scripts:: [
    ],
    _browser_action:: {},

    manifest_version: 2,
    name: name,
    description: description,
    version: version,
    icons: it._icons,
    browser_action: it._browser_action,
    content_security_policy: "script-src 'self'; object-src 'self';",
    omnibox: {
      keyword: keyword,
    },
    background: {
      scripts: it._background_scripts,
    },
    addBackgroundScripts(script):: self + {
      _background_scripts+: if std.isArray(script) then script else [script],
    },
  }
}
