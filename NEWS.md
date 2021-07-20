# fedmatch 2.0.1

- Fixed bugs in Makevars so that C++ code runs in parallel
- Fixed bugs in `tier_match` that stopped matches from being removed in between tiers
- Fixed bugs preventing the successful build on other platforms besides native linux

# fedmatch 2.0.0

- Added full data.table functionality to dramatically increase speed
- Added 'multivar' matching to match on multiple variables
  - Added capability to train a logit model and use the output
- Added 'Weighted Jaccard' match to get better matches
