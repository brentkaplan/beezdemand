# beezdemand LLM Documentation

Structured reference files for LLM-assisted coding with the beezdemand R package.

## Files in This Directory

| File | Purpose |
|------|---------|
| `docs-map.md` | Concise function map and vignette index (~60 lines) |
| `cheatsheet.md` | Code-heavy quick reference with runnable examples (~150 lines) |

## Access from R

```r
system.file("llm/cheatsheet.md", package = "beezdemand")
system.file("llm/docs-map.md", package = "beezdemand")
```

## Related Files (Not Installed)

These files live in the repo but are not included in the installed package:

- **`LLMS.md`** (repo root) — Short API card: entry points, data shape, return contracts, pitfalls
- **`docs/llms.txt`** — pkgdown-generated full function index (auto-generated, do not edit)

## Maintenance

- `docs-map.md` and `cheatsheet.md` are manually maintained
- Update these files when public API functions are added, renamed, or removed
- `docs/llms.txt` is regenerated automatically by `pkgdown::build_site()`
