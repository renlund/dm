% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expire_status.R
\name{expire_status}
\alias{expire_status}
\title{change active status at expiration boundary}
\usage{
expire_status(x, expire, slim = FALSE)
}
\arguments{
\item{x}{data frame with id variable 'id' and time variable 't'}

\item{expire}{expiration boundary}

\item{slim}{remove unnecessary statuses}
}
\value{
a data frame
}
\description{
Note: this is a rigid function that needs variables 'id' and 't' and will
    create a variable 'status' (if such exists it will be overwritten). Feed
    this function a data frame with variables 'id' (identifier) and 't'
    (integer valued time measurement). 't' represents times for some event
    whose status (1) should expire (to 0) (strictly) after the integer
    duration specified by 'expire' (unless the status is "refreshed" by
    having another 't'-value before expiration). Duplicated times (per
    individual) will always be removed, but the output can also be reduced
    to the smallest output that keeps track of the changing status by
    setting parameter 'slim' to \code{TRUE}.
}