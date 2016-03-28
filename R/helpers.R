HELPERTHREEJS_CLASS <- "helperThreeJS"

#' @rdname helper
#' @aliases helper boundingBox axisHelper gridHelper helperThreeJS
#' @title S3 class for threeJS helpers
#'
#' @description A generic threeJS helper definition. Helpers can be "global" or "mesh-specifc"
#'
#' @param builder Javascript function that builds the helper
#' @param type Either global or mesh
#' @param color Helper color in RGB format
#' @param size  Helper size
#'
#' @examples
#'  obj.helper <- boundingBox()
#'  obj <- objThreeJS(sphereGeometry(1, position = c(1,1,1)),
#'      lambertMaterial(color = "#FFFFFF"), helpers = obj.helper)
#'  grid <- gridHelper(center = c(0,-1,0))
#'  axis <- axisHelper(size = 10)
#'
helperThreeJS <- function(builder,
                          type = c("global","mesh"),
                          color = "#FFFFFF",
                          size = 1
){

  ##############
  # Validate parameters
  #################

  # Builder
  if ( !("JS_EVAL" %in% class(builder)) ) stop("Helper builder is not a javascript function")

  # type
  type <- match.arg(type)

  # Color
  tryCatch(is.matrix(col2rgb(color)),
           error = function(e) stop("Invalid color") )

  # size
  if (!is.numeric(size) || (size < 0) ) stop("Size should be a positive numeric value")

  #Create object
  ret <- list(
    buildFunc = builder,
    type = type,
    color = color,
    size = size
  )

  #Asign class and return
  class(ret) <- HELPERTHREEJS_CLASS

  return(ret)
}

#' @rdname helper
#' @export
is.helperThreeJS <- function(obj){
  if ( inherits(obj,HELPERTHREEJS_CLASS)  ) return(TRUE)
  else if ( is.list(obj)  )
    return(all(sapply(obj, function(x) HELPERTHREEJS_CLASS %in% class(x) )))
  else
    return( FALSE )
}

#' @rdname helper
#' @export
toJSONList.helperThreeJS <- function(obj){

  # Create parameter list (add color)
  params <- JSONThreeArgList(obj)

  # Create list
  ret <- list(
    buildFunc = obj$buildFunc,
    buildArgs = params,
    type = obj$type
  )

  ret <- dropNullEmpty(ret)

  # Return list
  return(ret)
}

###################################################################
#                               HELPERS
###################################################################

### CLASSES
BOUNDINGBOX_CLASS <- "boundingBox"
AXISHELPER_CLASS <- "axisHelper"
GRIDHELPER_CLASS <- "gridHelper"

# BUILDERS
helper.builder <- list()
helper.builder[[ BOUNDINGBOX_CLASS  ]] <- htmlwidgets::JS("Composition3D.helpers.bboxWrapper")
helper.builder[[ AXISHELPER_CLASS  ]] <- htmlwidgets::JS("Composition3D.helpers.axisWrapper")
helper.builder[[ GRIDHELPER_CLASS  ]] <- htmlwidgets::JS("Composition3D.helpers.gridWrapper")


############ Bounding box ###############

#' @rdname helper
#'
#' @details \code{boundingBox} Creates a bounding box instance. This helper is mesh(object)-specific
#'
#' @param diagonals If true, diagonals on each face of the box are included
#'
#' @export
boundingBox <- function(color = "#FFFF00", diagonals = F ){

  # Type is mesh since bounding box is associated with a mesh
  ret <- helperThreeJS( helper.builder[[ BOUNDINGBOX_CLASS  ]], type = "mesh", color = color )

  # Diags
  if (!is.logical(diagonals)) stop("Diagonals should be a boolean value")

  ret$diagonals <- diagonals

  # Append class
  class(ret) <- append(class(ret), BOUNDINGBOX_CLASS)

  # Return
  return( ret )
}

JSONThreeArgList.boundingBox <- function(obj){
  # Return as an obj
  return( list(obj$color, obj$diagonals) )
}


################# AXIS HELPER ################

#' @rdname helper
#'
#' @details \code{axisHelper} Creates a Axis helper instance. This is a global helper (not associated with a specific object)
#'
#' @param size Numeric value. axis length
#' @param center 3D vector that locates the center of the helper
#'
#' @export
axisHelper <- function(size =  25, center = c(0,0,0) ){

  # Is global ( applicable to a scene )
  ret <- helperThreeJS( helper.builder[[ AXISHELPER_CLASS  ]], type = "global", size = size )

  if ( !is.numeric(center) || length(center) != 3 ) stop("Center should be a 3D numeric vector")

  ret$center = center

  # Append class
  class(ret) <- append(class(ret), AXISHELPER_CLASS)

  # Return
  return( ret )

}


JSONThreeArgList.axisHelper <- function(obj){
  # Return as an obj
  return( list(obj$size, obj$center) )
}


################# GRID HELPER ################

#' @rdname helper
#'
#' @details \code{gridHelper} Creates a Grid helper. This helper is not linked to a specific object
#'
#' @param size Numeric value. Grid size
#' @param step Numeric value. Size of the step between two lines
#' @param gridColor Color of the lines of the grid.
#' @param midColor Color of the centerline.
#' @param Rotation 3D vector that specifies the x,y,z rotations
#'
#' @export
gridHelper <- function(size =  10, step = 1 , gridColor = "#FFFFFF", midColor = "#FFFF00", center = c(0,0,0), rotation = c(0,0,0) ){

  # Is global ( applicable to a scene )
  ret <- helperThreeJS( helper.builder[[ GRIDHELPER_CLASS  ]], type = "global", size = size, color = gridColor )

  # Add step and centercolor
  if ( !is.numeric(step) || (step < 0) ) stop("Step should be a positive numeric value")

  # Color
  tryCatch(is.matrix(col2rgb(midColor)),
           error = function(e) stop("Invalid centerline color") )

  # Center and rotation
  if ( !is.numeric(center) || length(center) != 3 ) stop("Center should be a 3D numeric vector")
  if ( !is.numeric(rotation) || length(rotation) != 3 ) stop("Center should be a 3D numeric vector")

  ret$step = step;
  ret$centerColor = midColor;
  ret$center = center;
  ret$rotation = rotation;

  # Append class
  class(ret) <- append(class(ret), GRIDHELPER_CLASS)

  # Return
  return( ret )

}

JSONThreeArgList.gridHelper <- function(obj){
  # Return as an obj
  return( list(obj$size, obj$step, obj$color, obj$centerColor, obj$center, obj$rotation) )
}
