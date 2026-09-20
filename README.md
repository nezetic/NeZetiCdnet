# NeZetiC.net

Static site generator and content for [nezetic.net](https://www.nezetic.net), built
with [Hakyll](https://jaspervdj.be/hakyll/).

## Requirements

* [Stack](https://docs.haskellstack.org/) (the toolchain/GHC version is pinned in
  `stack.yaml` via the LTS resolver).
* [minify](https://github.com/tdewolff/minify) (`minify` on `PATH`). Every
  generated HTML page and every `js/*.js` file is piped through `minify`
  (`--type=html` / `--type=js`) at build time, so a missing binary fails the
  build.

## Building

```sh
stack build                     # build the generator
stack exec nezetic.net -- build # generate the site into _site/
stack exec nezetic.net -- watch # rebuild on change and serve locally
stack exec nezetic.net -- clean # remove _cache/ and _site/
```

The `Makefile` wraps these common commands (`make`, `make watch`, `make deploy`).

## Linting

The generator sources are linted with [HLint](https://github.com/ndmitchell/hlint),
configured by `.hlint.yaml`:

```sh
make lint   # report hints (non-zero exit if any are found)
make check  # build the site and lint in one go
```

HLint is pulled from the Stackage snapshot on first use; to auto-apply the
mechanical suggestions run:

```sh
stack exec --package hlint -- hlint --refactor --refactor-options="--inplace" src
```

## Layout

| Path            | Contents                                                        |
| --------------- | --------------------------------------------------------------- |
| `posts/`        | Blog posts, Markdown with YAML front matter                     |
| `pages/`        | Standalone pages (`about`, `contact`, ...)                      |
| `projects/`     | Project pages                                                   |
| `collection/`   | Older hardware write-ups                                        |
| `templates/`    | Hakyll HTML templates                                           |
| `robots.txt`    | Web crawler rules; points at the generated `sitemap.xml`        |
| `css/`          | Stylesheets (minified at build time)                            |
| `js/`           | Client-side JavaScript                                           |
| `images/`, `files/` | Static assets                                                |
| `src/`          | The Haskell generator (Hakyll rules, contexts, compilers)        |

Posts use a `<!--MORE-->` marker to split the teaser shown on the home page from
 the full body.

## Languages

The site is currently available in French and English. French is the default
language and is served from the root (`/posts/...`, `/about/`, ...); English is
served below the `/en/` prefix (`/en/posts/...`, `/en/about/`, ...).

Each language has its own content directories, mirroring the default language:

| Path                | Contents                                        |
| ------------------- | ----------------------------------------------- |
| `en/posts/`         | English blog posts (same file names as `posts/`) |
| `en/pages/`         | English standalone pages                         |
| `en/projects/`      | English project pages                            |
| `en/sidebar.markdown` | English sidebar                                |

A language is declared in `src/Languages.hs`, which also holds its UI strings
(headings, `Read more`, feed title, ...). The language switcher links to the
translated version of the current page when it exists, and falls back to the
other language's home page otherwise. Adding or removing a translated file is
picked up automatically on the next build.

Static assets (`images/`, `files/`, `css/`, `js/`) are shared by all languages.
