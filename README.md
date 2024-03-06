# actions

Collection of handy GitHub actions.

## Usage

### [Polar](https://polar.sh/): strip paywall

When publishing articles with paywalled snippets (as benefit for paying subscribers), this action will remove them after X days since publication.

``` yaml
jobs:
  PolarStripPaywalls:
    timeout-minutes: 15
    runs-on: ubuntu-22.04
    steps:
      - name: StripIt
        uses: eval/actions/polar-strip-paywall@main
        with:
          org: your-org
          days-since-publish: 7
        env:
          POLAR_API_TOKEN: ${{ secrets.POLAR_API_TOKEN }}
```

See also this [dogfooding workflow](https://github.com/eval/actions/blob/main/.github/workflows/polar-strip-paywall.yml).  
See [CLI documentation](./polar-strip-paywall/README.md).

### [Polar](https://polar.sh/): tag posts

[Example](https://polar.sh/eval/posts/articles-by-tag):

![Screenshot 2024-03-06 at 11 10 53](https://github.com/eval/actions/assets/290596/7e7edd33-e332-4c79-8ed9-00fa8aed25e8)

#### Usage

1. Tag a post  
  Insert `<!-- POLAR-TAGS tags="tag1, some other tag" -->` in a post.  
  **NOTE**: Make sure to use double quotes even when having one tag.
1. Create a post listing all tags  
  ...insert `<!-- POLAR-TAGS-LIST -->` and publish.
1. Run the following job

``` yaml
jobs:
  PolarTags:
    timeout-minutes: 15
    runs-on: ubuntu-22.04
    steps:
      - name: Update tag-snippets
        uses: eval/actions/polar-tags@main
        with:
          org: your-org
        env:
          POLAR_API_TOKEN: ${{ secrets.POLAR_API_TOKEN }}
```

See also this [dogfooding workflow](https://github.com/eval/actions/blob/main/.github/workflows/polar-tags.yml).  
See [CLI documentation](./polar-tags/README.md).
