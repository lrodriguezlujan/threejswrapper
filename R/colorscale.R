COLORSCALETHREEJS_CLASS <- "colorscaleThreeJS"

#' @rdname colorscale
#' @aliases colorscale colorscaleThreeJS
#'
#' @title S3 class for threeJS LUT colorscale
#'
#' @description An R-side object of a JS LUT colorscale. It includes a set of predefined color maps,
#' and a huge ammount of parameters. Color legend can be added.
#'
#' @param colorMap Color scale mapping
#' @param numcolors Number of different colors
#' @param max Maximum value in the scale
#' @param min Minimim value in the scale
#' @param legend.on If \code{TRUE} a colorbar legend is added to the scene
#' @param legend.layout Either vertical or horizontal
#' @param legend.position Legend position in the range [0,1] x [0,1] in top-left to bottom-right orientation
#' @param legend.dimensions Legend size in the x,y dimensions from 0 to 1
#' @param legend.labels.on Add labels to the ticks in the legend
#'
#' @examples
#' \dontrun{
#' #colorscaleThreeJS(
#' #max = 1, min = -1, legend.on = T, legend.labels.on = T,
#' #legend.labels.title = 'EXAMPLE')
#' runShinyExample("parametric")
#'}
#'
#' @export
colorscaleThreeJS <- function(colorMap = c("rainbow", "cooltoworm", "blackbody", "greyscale"),
                               numcolors = 512,
                               max = 1,
                               min = 0,
                               legend.on = F,
                               legend.layout = c('vertical', 'horizontal'),
                               legend.position = c( 0.9, 0.8 ),
                               legend.dimensions = c( 0.05, 0.3),
                               legend.labels.on = F,
                               legend.labels.size = 24,
                               legend.labels.font = 'Arial',
                               legend.labels.title = '',
                               legend.labels.titleColor = 'white',
                               legend.labels.um = NULL,
                               legend.labels.nticks = 8,
                               legend.labels.tickColor = 'white',
                               legend.labels.labelColor = 'white',
                               legend.labels.decimal = 2,
                               legend.labels.notation = c('standard', 'scientific')
){

  ##############
  # Validate parameters
  #################

  # Colormap
  colorMap <- match.arg(colorMap)

  # Numcolors
  if (!is.numeric(numcolors) || floor(numcolors) != numcolors || numcolors <= 0 ) stop("")

  # Max / min
  if ( !is.numeric(max) || !is.numeric(min) ) stop("")
  if ( max < min ) {
    warning("")
    aux <- max
    max <- min
    min <- aux
  }

  # Legend.on
  if (!is.logical(legend.on)) stop("")

  if (legend.on) {
    # legend.layout
    legend.layout <- match.arg(legend.layout)

    # legend.position
    if (!is.numeric(legend.position) || length(legend.position) != 2 || any(legend.position < 0 ) ) stop("")

    # legend.dimension
    if (!is.numeric(legend.dimensions) || length(legend.dimensions) != 2 || any(legend.dimensions < 0 )) stop("")

    # Labels on
    if (!is.logical(legend.labels.on)) stop("")

    if (legend.labels.on) {

      # label size
      if (!is.numeric(legend.labels.size) || floor(legend.labels.size) != legend.labels.size || legend.labels.size <= 0 ) stop("")

      #font
      if (!is.character(legend.labels.font)) stop("")

      # title
      if (!is.character(legend.labels.title)) stop("")

      # Um
      if (!is.null(legend.labels.um) && !is.character(legend.labels.um)) stop("")

      # Nticks
      if (!is.numeric(legend.labels.nticks) || floor(legend.labels.nticks) != legend.labels.nticks || legend.labels.nticks < 0 ) stop("")

      # Decimal
      if (!is.numeric(legend.labels.decimal) || floor(legend.labels.decimal) != legend.labels.decimal || legend.labels.decimal < 0 ) stop("")

      # Notation
      legend.labels.notation <- match.arg(legend.labels.notation)
    }
  }

  if (legend.on) {

    names(legend.position) <- c("x","y")
    names(legend.dimensions) <- c("width", "height")
    legend <- list(layout = legend.layout, position = as.list(legend.position), dimensions = as.list(legend.dimensions) )

    if (legend.labels.on)
    {
      legend$labels <- list(fontsize = legend.labels.size, fontface = legend.labels.font, title = legend.labels.title, titleColor = legend.labels.titleColor,
                            um = legend.labels.um, ticks = legend.labels.nticks, decimal = legend.labels.decimal,
                            tickColor = legend.labels.tickColor, labelColor = legend.labels.labelColor, notation = legend.labels.notation)
      # Drop nulls
      legend$labels <- dropNullEmpty(legend$labels)
    }
    legend <- dropNullEmpty(legend)
  }
  else
    legend <- NULL

  #Create object
  ret <- list(
    colorMap = colorMap,
    ncolors = numcolors,
    max = max,
    min = min,
    legend = legend
  )

  # Drop nulls
  ret <- dropNullEmpty(ret)

  #Asign class and return
  class(ret) <- COLORSCALETHREEJS_CLASS

  return(ret)
}

#' @rdname colorscale
#'
#' @export
is.colorscaleThreeJS <- function(obj){
  if ( inherits(obj,COLORSCALETHREEJS_CLASS)  ) return(TRUE)
  else if ( is.list(obj)  )
    return(all(sapply(obj, function(x) COLORSCALETHREEJS_CLASS %in% class(x) )))
  else
    return( FALSE )
}

#' @rdname colorscale
#'
#' @export
toJSONList.colorscaleThreeJS <- function(obj){

  # Return as is
  return(obj)
}
