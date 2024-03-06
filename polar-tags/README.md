# Polar tags

CLI that aids tagging Polar posts.

## Prerequisites

* [Babashka](https://github.com/babashka/babashka#installation)
* ENV-var `POLAR_API_TOKEN` via https://polar.sh/settings

## Usage

1. Tag a post
  Insert `<!-- POLAR-TAGS tags="tag1, some other tag" -->` in a post.
  NOTE: Make sure to use quotes even when having one tag.
2. Creat a tag overview page
  ...and insert `<!-- POLAR-TAGS-LIST -->`.
3. Run this CLI
  It replaces the POLAR-TAGS-comment with something like `Tags: [#tag1](https://polar.sh/some-org/posts/tag-overview-page#tag1), ,,,`.
  And replaces the POLAR-TAGS-LIST-comment with a listing of every article by tag.

  Rerun on every change

``` shell
$ bb polar-tags.bb -h
CLI that...
 1) replaces `<!-- POLAR-TAGS tags="tag1, tag2" -->` with links to tags-page
    in all (unpublished) posts
 2) replaces `<!-- POLAR-TAGS-LIST -->` with a list of posts per tag

Usage: bb polar-tags [OPTIONS]

OPTIONS
      --org     organization_name (required)
      --dry-run Print candidate articles, don't make changes.
  -h, --help

ENVIRONMENT VARIABLES
  POLAR_API_TOKEN    token from https://polar.sh/settings
```
