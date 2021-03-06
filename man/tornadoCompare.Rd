% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/textFunctions.R
\name{tornadoCompare}
\alias{tornadoCompare}
\title{Generates tornado plot}
\usage{
tornadoCompare(termmatrix, pickword, frequency, nwords)
}
\arguments{
\item{termmatrix}{tm TermDocumentMatrix object}

\item{pickword}{a vector of words to select based on}

\item{frequency}{how often should the words occur in the document}

\item{nwords}{how many frequent words should be graphed?}
}
\value{
a ggplot2 object
}
\description{
This function makes a tornado plot comparing documents based on selected word frequencies. For example, can compare documents containing the word "gender" to all other documents
}
\examples{
interest_plot_bydoc("research",TermDocumentMatrix(corpus1))
}
\seealso{
\code{\link{ggplot2}}
}

