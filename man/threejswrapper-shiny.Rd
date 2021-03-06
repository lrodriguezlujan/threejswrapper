% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scene.R, R/threejswrapper.R
\name{sceneOutput}
\alias{renderScene}
\alias{sceneOutput}
\alias{threejswrapper-shiny}
\title{Shiny bindings for threejswrapper}
\usage{
sceneOutput(outputId, width = "100\%", height = "400px")

renderScene(expr, env = parent.frame(), quoted = FALSE)
}
\arguments{
\item{outputId}{output id in the shiny UI}

\item{width, height}{Must be a valid CSS unit}

\item{expr}{An expression that generates the graphics (\see{scene})}

\item{env}{The environment in which to evaluate \code{expr}.}

\item{quoted}{Is \code{expr} a quoted expression}
}
\description{
Output and render functions for shiny
}

