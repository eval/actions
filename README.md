# actions

Collection of handy GitHub actions.

## Usage

### Polar: strip paywall

When publishing articles including a paywall (as benefit for paying subscribers), this action will remove them after X days since publication.

``` yaml
jobs:
  PolarStripPaywalls:
    timeout-minutes: 15
    runs-on: ubuntu-22.04
    steps:
      - name: StripIt
        uses: eval/actions/polar-strip-paywall@main
        with:
          org: eval
          days-since-publish: 7
        env:
          POLAR_API_TOKEN: ${{ secrets.POLAR_API_TOKEN }}
```
