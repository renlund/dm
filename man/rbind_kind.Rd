% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rbind_kind.R
\name{rbind_kind}
\alias{rbind_kind}
\title{forgiving rbind version}
\usage{
rbind_kind(x, y, in.both = FALSE)
}
\arguments{
\item{x}{a data frame}

\item{y}{another data frame}

\item{in.both}{if TRUE, only columns present in both 'x' and 'y' are rbinded}
}
\description{
rbind where columns do not necessarily are equal
}
