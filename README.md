
# Cross crisis analysis

<!-- badges: start -->
<!-- badges: end -->

The goal of pa-cross-crisis is to compare globally available datasets on the
severity of humanitarian situations and the risk of crisis deterioration.

## Setup

To store data, set the path to the `Publications/cross_crisis/data` folder as
an environment variable `CC_DATA_DIR`. It's easy to do in R with
`usethis::edit_r_environ()`:

```shell
CC_DATA_DIR="/your/local/path"
```

## Data sources

The data sources are pulled together in a file under `wrangled` unless otherwise
specified below:

1. IRC Emergency Watchlist (manually curated)
2. INFORM Severity