{
  new(name, keyword, description, version, service_worker):: {
    local it = self,
    _icons:: {},
    _background_scripts:: [
    ],
    _browser_action:: {},

    manifest_version: 3,
    name: name,
    description: description,
    version: version,
    icons: it._icons,
    browser_action: it._browser_action,
    content_security_policy: {
      extension_pages: "script-src 'self'; object-src 'self';"
    },
    omnibox: {
      keyword: keyword,
    },
    background: {
      service_worker: service_worker,
    },
    addIcons(icons):: self + {
      _icons+: icons,
    },
  }
}
