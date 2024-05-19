# fedmatch 2.0.6
- Fixed documentation referencing "X" to be to the proper vignette
- Fixed documentation referencing "wgt_jaccard_distance" that should be "wgt_jaccard_dist"
- Fixed issue so that multivar_match warns when invalid compare_types are used
- Fixed issue where wgt_jaccard_dist wasn't listed as a valid compare type
- Fixed bug where 'difference' comparison method would assign wrong scores for negative numbers
- Improved documentation around scoring for post-hoc matchscores


# fedmatch 2.0.5
- Fixed bug in `build_tier_settings` that didn't include `sequential_words` argument
- Fixed bug where `clean_strings` wasn't removing the replacement words even if `remove_words == T`
- Fixed bug where `tier_match` didn't work in some cases with `fuzzy_match`
- Now exporting `build_corpus` to easily compute `wgt_jaccard_distance`
- Added weighted jaccard distance option to `compare_type` in multivar matching

# fedmatch 2.0.4
- Added `wgt_jaccard_distance` to easily compute Weighted Jaccard Distances for two string vectors
- Lowered R requirement to 3.5.3
- Added behavior to remove NA observations from exact matches, reinserting them into later tiers if necessary
- Fixed bug in `tier_match` building 'nomatch' datasets incorrectly
- Fixed bug in `wgt_jaccard` where inverse frequencies were not being logged.

# fedmatch 2.0.3

- Fixed STATE_FIPS to actually include FIPS codes
- Changed the output column from 'multivar' matching to be called 'multivar_score' rather than 'matchscore', so as not to conflict with score_settings in `merge_plus`
- Fixed bug in Weighted Jaccard scoring that would allow scores > 1 if the two names shared multiples of the same word


# fedmatch 2.0.2

- Fixed bugs in Weighted Jaccard code to run on MacOS environments

# fedmatch 2.0.1

- Fixed bugs in Makevars so that C++ code runs in parallel
- Fixed bugs in `tier_match` that stopped matches from being removed in between tiers
- Fixed bugs preventing the successful build on other platforms besides native Linux

# fedmatch 2.0.0

- Added full data.table functionality to dramatically increase speed
- Added 'multivar' matching to match on multiple variables
  - Added capability to train a logit model and use the output
- Added 'Weighted Jaccard' match to get better matches
