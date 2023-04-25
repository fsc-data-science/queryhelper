# querymatch

A simple term frequency model for finding Flipside Crypto SQL queries to fork.

This repo contains the code (but not the query database or model) used in the Querymatch API 
that powers our [QSearch app](https://science.flipsidecrypto.xyz/qsearch/).

For a deeper dive into the context, you can check out our beehiiv write-up  [query-match](https://science.flipsidecrypto.xyz/query-match/).

# Reproduce Analysis

All analysis is reproducible using the R programming language. You'll need (1) an shroomDK API key to copy our SQL queries and extract data from the [FlipsideCrypto data app](https://next.flipsidecrypto.xyz/); and (2) renv to get the exact package versions we used.

## shroomDK

shroomDK is an R package that accesses the FlipsideCrypto REST API; it is also available for Python. You pass SQL code as a string to our API and get up to 1M rows of data back!

Check out the [documentation](https://docs.flipsidecrypto.com/shroomdk-sdk/get-started) and get your free API Key today.

## renv

renv is a package manager for the R programming language. It ensures analysis is fully reproducible by tracking the exact package versions used in the analysis.

`install.packages('renv')`

## Instructions

To replicate this analysis please do the following:

1.  Clone this repo.
2.  Reach out to us to get the Querymatch API URL endpoint and save it in the qsearch folder as `qmatch_url.txt` so your local copy of the app can request matched queries.
3.  Open the `queryhelper.Rproj` R Project file in your R IDE (we recommend, RStudio).
4.  Confirm you have renv installed.
5.  Restore the R environment using `renv::restore()` while in the `queryhelper` R Project.
6.  You can now run `qsearch/ui.R`

If any errors arise, double check you have saved the `qmatch_url.txt` in the expected file name and format.

