% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dm_create.R
\name{dm_create}
\alias{dm_create}
\title{Create data frame from documentation info}
\usage{
dm_create(set, id.name, doc = NULL)
}
\arguments{
\item{set}{vector of id's}

\item{id.name}{identifier across data bases or a list of such, with names
equal to the associated data base}

\item{doc}{if \code{NULL}, we look at \code{dm_doc()}}
}
\description{
use the information stored (by default accessibly in
    \code{dm_doc()}) to create a data frame
}
