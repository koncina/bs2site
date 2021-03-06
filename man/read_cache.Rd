% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cache_rds.R
\name{read_cache}
\alias{read_cache}
\title{Read objects from the website template cache folder}
\usage{
read_cache(object = NULL, cache_folder = "_cache")
}
\arguments{
\item{object}{object to load. If missing, `read_cache()` will load all objects found in the cache folder.}

\item{cache_folder}{defaults to `_cache` subfolder.}
}
\value{
`read_cache()` returns the relative path to the rds file invisibly.
}
\description{
Use this function to load objects from rds files saved in a standardised location.
}
