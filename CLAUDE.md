# Running the pipeline locally

The project is pinned to R 4.4.3 (see `renv.lock`). The default R on this machine is newer (4.6), which can't build the locked packages (e.g. Matrix fails to compile). Always run R for this project through [rig](https://github.com/r-lib/rig) with the 4.4 install, and don't change the system default R:

```sh
rig run -r 4.4-arm64 -e 'targets::tar_make()'
```

## Restoring packages

`renv::restore()` fails on `data.table` here: it builds from source against Homebrew's libomp and the result won't load (`___kmpc_dispatch_deinit`). Workaround:

1. Download `data.table_1.18.4.tar.gz` from the CRAN Archive and, in `configure`, change `HOMEBREW_PREFIX=/opt/homebrew` to `HOMEBREW_PREFIX=/nonexistent` (builds without OpenMP).
2. `rig run -r 4.4-arm64 -e 'renv::install("<path to patched tarball>")'`
3. `rig run -r 4.4-arm64 -e 'renv::restore(exclude = "data.table")'`

Don't change `renv.lock` for this. `renv::status()` will report the project as out-of-sync afterwards; that's expected.

Source builds also need `~/.R/Makevars` to include the Homebrew gettext paths (`CPPFLAGS += -I/opt/homebrew/opt/gettext/include`, `LDFLAGS += -L/opt/homebrew/opt/gettext/lib`).

## Refreshing 17Lands data

`record_raw` is cached by targets, so `tar_make()` alone uses stale data. To re-download (needs credentials in `.Renviron`):

```sh
rig run -r 4.4-arm64 -e 'targets::tar_invalidate(record_raw); targets::tar_make()'
```

## Manual corrections

`manual_corrections.csv` rows must match the 17Lands record exactly on `datetime` (UTC, including seconds), `set_code`, `deck_color` and `format`. Copy the values from `targets::tar_read(record_clean)` rather than from the 17Lands page, which shows local time and no seconds. Unmatched corrections are warned about and ignored.
