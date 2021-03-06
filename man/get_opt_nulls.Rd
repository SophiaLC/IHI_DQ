% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_opt_nulls.R
\name{get_opt_nulls}
\alias{get_opt_nulls}
\title{Getting Nulls Summaries for Optional Fields}
\usage{
get_opt_nulls(data)
}
\arguments{
\item{data}{The raw data from BioSense on which you will do the null checks.}
}
\value{
A summary data frame that lists counts and percentages for null fields, summarized at facility-level.
}
\description{
This will generate percent and counts of null message, by facility. All are calculated on a once-per-visit basis (see `?get_req_nulls`).
}
\details{
The distinctions between optional vs. invalid and per visit vs. every message were made based off of our interpretation of the
PHIN guide as well as the needs of the Kansas Department of Health and Environment Syndromic Surveillance.
}
