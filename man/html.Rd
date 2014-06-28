\docType{methods}
\name{html,partition-method}
\alias{html}
\alias{html,partition-method}
\alias{html-method}
\title{Convert partition into html}
\usage{
\S4method{html}{partition}(object, filename = NULL, type = "debate")
}
\arguments{
  \item{object}{a partition object}

  \item{filename}{filename for the html file, if NULL
  (default), a temporary file is created}

  \item{type}{the type of html to be generated}
}
\description{
A partition is converted into a html file and displayed in
a browser
}

