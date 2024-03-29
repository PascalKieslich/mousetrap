% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add.R
\name{mt_add_variables}
\alias{mt_add_variables}
\title{Add new variables to trajectory array.}
\usage{
mt_add_variables(data, use = "trajectories", save_as = use, variables)
}
\arguments{
\item{data}{a mousetrap data object created using one of the mt_import
functions (see \link{mt_example} for details). Alternatively, a trajectory
array can be provided directly (in this case \code{use} will be ignored).}

\item{use}{a character string specifying which trajectory data should be
used.}

\item{save_as}{a character string specifying where the resulting trajectory
data should be stored.}

\item{variables}{a list of matrices that each contain the data of
one of the to be added variables. In this case, the new variables with
their values are added as a new entry in the trajectory arrays third
dimension. Alternatively, a character vector specifying the name of the new
variables that should be added to the trajectory array. In this case, the
new variables are filled with \code{NA}s.}
}
\value{
A mousetrap data object (see \link{mt_example}) where the new
variables have been added to the trajectory array.
Depending on the input to \code{variables}, the values for the added
variables are either \code{NA}s or their actual values. If columns of the
same name already existed, they have been removed. If the trajectory array
was provided directly as \code{data}, only the trajectory array will be
returned.
}
\description{
Add new variables to the trajectory array (and remove potentially existing
variables of the same name). This is mostly a helper function used by other
functions in this package (e.g., \link{mt_deviations}). However, it
can also be helpful if the user has calculated new variables for each logged
coordinate and wants to add them to an existing trajectory array.
}
\examples{
# Calculate new (arbitrary) variables for this example
# ... the sum of the x- and y-positions
xy_sum <- mt_example$trajectories[,,"xpos"] + mt_example$trajectories[,,"ypos"]
# ... the product of the x- and y-positions
xy_prod <- mt_example$trajectories[,,"xpos"] * mt_example$trajectories[,,"ypos"]

# Add the new variables to the trajectory array
mt_example <- mt_add_variables(mt_example,
  variables=list(xy_sum=xy_sum, xy_prod=xy_prod))

}
\author{
Pascal J. Kieslich

Felix Henninger
}
