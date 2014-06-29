\name{controls}
\alias{controls}
\title{inspect or set drillingControls}
\usage{
controls(...)
}
\arguments{
  \item{...}{parameters you want to set}
}
\description{
Set drillingControls (a list in the global environment).
The controls are used by several functions to keep the
number of needed parameters short. WARNING: Providing a
character string length > 1 does not yet work!
}
\examples{
controls() # view the current setting of parameters
}

