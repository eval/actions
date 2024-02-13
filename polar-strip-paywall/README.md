# Polar strip paywall

## Prerequisites

* [Babashka](https://github.com/babashka/babashka#installation)
* ENV-var `POLAR_API_TOKEN` via https://polar.sh/settings

## Usage

``` shell
$ bb polar-strip-paywall.bb

Usage: polar-strip-paywall [OPTIONS]

OPTIONS
      --org                  organization_name (required)
      --days-since-publish 7
      --dry-run              Print candidate articles, don't make changes.
  -h, --help

ENVIRONMENT VARIABLES
  POLAR_API_TOKEN    token from settings
```
