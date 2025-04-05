
# functionThinking

<!-- badges: start -->
![](https://www.r-pkg.org/badges/version-last-release/r.proxy)
![](https://cranlogs.r-pkg.org/badges/grand-total/r.proxy)
![](https://cranlogs.r-pkg.org/badges/last-day/r.proxy)
![](https://cranlogs.r-pkg.org/badges/last-week/r.proxy)
![](https://cranlogs.r-pkg.org/badges/r.proxy)
<!-- badges: end -->

The goal of functionThinking is to run function with thinking.

## Installation

You can install the development version of functionThinking like so:

``` r
devtools::install_github("xiayh17/functionThinking")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(functionThinking)
chat <- chat_deepseek(
  base_url = Sys.getenv("base_url"),
  api_key = Sys.getenv("api_key"),
  model = Sys.getenv("model"),
  seed = NULL,
  api_args = list(),
  echo = NULL
)

fun.thinking((a = mean(1:10)), chat = chat)
```
