# Polar tags

CLI that aids tagging Polar posts. [Example](https://polar.sh/eval/posts/articles-by-tag):

![Screenshot 2024-03-06 at 11 10 53](https://github.com/eval/actions/assets/290596/7e7edd33-e332-4c79-8ed9-00fa8aed25e8)

## Prerequisites

* [Babashka](https://github.com/babashka/babashka#installation)
* ENV-var `POLAR_API_TOKEN` via https://polar.sh/settings
* a tagged article and a tag overview page (see Usage)

## Usage

1. Tag a post  
  Insert `<!-- POLAR-TAGS tags="tag1, some other tag" -->` in a post.  
  **NOTE**: Make sure to use double quotes even when having one tag.
1. Create a post listing all tags  
  ...insert `<!-- POLAR-TAGS-LIST -->` and publish.
1. Run the CLI  
  `$ bb polar-tags.bb --org your-org`  
  As shown in the screenshot above it replaces the POLAR-TAGS-comment with something like `Tags: [#tag1](https://polar.sh/some-org/posts/tag-overview-page#tag1), ,,,`, and replaces the POLAR-TAGS-LIST-comment with a listing of every article by tag.  

``` shell
$ bb polar-tags.bb -h
CLI that...
 1) replaces `<!-- POLAR-TAGS tags="tag1, tag2" -->` with links to tags-page
    in all (unpublished) posts
 2) replaces `<!-- POLAR-TAGS-LIST -->` with a list of posts per tag

Usage: bb polar-tags.bb [OPTIONS]

OPTIONS
      --org     organization_name (required)
      --dry-run Print articles that would change, don't make changes.
  -h, --help

ENVIRONMENT VARIABLES
  POLAR_API_TOKEN    token from https://polar.sh/settings
```
