
# Cross crisis analysis

<!-- badges: start -->
<!-- badges: end -->

The goal of pa-cross-crisis is to compare globally available datasets on the
severity of humanitarian situations and the risk of crisis deterioration.

## Setup

To store data, set the path to the `Publications/cross_crisis` folder as
an environment variable `CC_DIR`. It's easy to do in R with
`usethis::edit_r_environ()`:

```shell
CC_DIR="/your/local/path"
```

Then input files (that need wrangling) are available in the `cc_dir/input`
folder and wrangled files in `cc_dir/data`.

## Data sources

The data sources are pulled together in a file under `wrangled` unless otherwise
specified below:

1. IRC Emergency Watchlist (manually curated)
2. [INFORM Severity](wrangling/inform_severity.R)
3. [INFORM Risk](wrangling/inform_risk.R)
4. [CERF CIRV](wrangling/cerf_cirv.R)
5. [OCHA PINs](wrangling/ocha_pins.R)