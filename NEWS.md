# bs2site 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
- Added `pkgdown` documentation
- Added box colour definitions: intro, warning, practical... (requires `iosp` released after commit [11c9ab7](https://github.com/koncina/iosp/commit/11c9ab7bfa97dfe07c2fc9368de31d89379dbf4f)). Run `use_box_colours()` to setup the definitions.

- Update the `create_navbar()` function to allow keeping the navbar title in addition to branding logos.
- Navbar can be generated for pages in subfolders (new `relative_to` argument).
- Fixed bug with empty list entries (unnecessary tibble coercion).

