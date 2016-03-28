#' Interactive 3D plots for R using three.js and htmlwidgets.
#'
#' Interactive 3D plots for R using three.js and htmlwidgets.
#'
#' @name threejswrapper-package
#' @references
#' \url{http://threejs.org}
#' @aliases threejswrapper
#' @examples
#' \dontrun{
#'  runShinyExample("spheres")
#'  runShinyExample("cylinders")
#'  runShinyExample("torus")
#'  runShinyExample("mesh")
#'  runShinyExample("parametric")
#' }
#' @seealso \link{scene} \link{threejswrapper-shiny}
#'
#' @docType package
NULL


#' @name threejswrapper-shiny
#'
#' @title Shiny bindings for threejswrapper
#'
#' @description Output and render functions for shiny
#'
#' @param outputId output id in the shiny UI
#' @param width,height Must be a valid CSS unit
#' @param expr An expression that generates the graphics (\see{scene})
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression
#'
#' @importFrom htmlwidgets shinyWidgetOutput
#' @importFrom htmlwidgets shinyRenderWidget
NULL
