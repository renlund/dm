% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dm_find.R
\name{dm_find}
\alias{dm_find}
\title{find info in variables}
\usage{
dm_find(pattern, ignore.case = TRUE, attrib = "label", ..., verbose = TRUE)
}
\arguments{
\item{pattern}{search string}

\item{ignore.case}{logical}

\item{attrib}{name of attribute to search in (default 'label')}

\item{...}{arguments passed to \code{grepl}}

\item{verbose}{logical}
}
\description{
look for a search string in variable names and label (or other) attribute
}
