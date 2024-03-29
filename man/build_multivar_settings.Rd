% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_multivar_settings.R
\name{build_multivar_settings}
\alias{build_multivar_settings}
\title{Build settings for multivar matching}
\usage{
build_multivar_settings(
  logit = NULL,
  missing = FALSE,
  wgts = NULL,
  compare_type = "diff",
  blocks = NULL,
  blocks.x = NULL,
  blocks.y = NULL,
  top = 1,
  threshold = NULL,
  nthread = 1
)
}
\arguments{
\item{logit}{a glm or lm model as a result from a logit regression on a verified dataset. See details.}

\item{missing}{boolean T/F, whether or not to treat missing (NA) observations as its own binary column for each column in by. See details.}

\item{wgts}{rather than a lm model, you can supply weights to calculate matchscore. Can be weights from \code{calculate_weights}.}

\item{compare_type}{a vector with the same length as "by" that describes how to compare the variables. Options are "in", "indicator", "substr", "difference", "ratio", "stringdist", and "wgt_jaccard_dist". See the Multivar Matching Vignette for details.}

\item{blocks}{variable present in both data sets to "block" on before computing scores. Matchscores will only be computed for observations that share a block. See details.}

\item{blocks.x}{name of blocking variables in x. cannot supply both blocks and blocks.x}

\item{blocks.y}{name of blocking variables in y. cannot supply both blocks and blocks.y}

\item{top}{integer. Number of matches to return for each observation.}

\item{threshold}{numeric. Minimum score for a match to be included in the result.}

\item{nthread}{integer. Number of cores to use when computing all combinations. See \code{parallel::makecluster()}}
}
\value{
a list containing options for the 'multivar_settings' argument of \code{merge_plus}.
}
\description{
\code{build_multivar_settings} is a convenient way to build the list for the multivar settings argument in merge_plus
}
