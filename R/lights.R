LIGHTTHREEJS_CLASS <- "lightThreeJS"

#' @rdname light
#' @title  ThreeJS Lights
#' @aliases light lightThreeJS ambientLight directionalLight
#'
#' @description Light implementations such as Ambient or Directional lighting.
#'
#' @param builder Javascript function that builds the lights
#' @param color Light color in RGB format
#'
#' @examples
#' light <-
#' list(
#'  directionalLight(
#'    "#FFFFFF",intensity = 0.5,from = c(1,1,0)
#'  ),
#'  directionalLight(
#'    "#FFFFFF",intensity = 0.5,from = c(0,0,1)
#'    #'  ),
#'  ambientLight("#333333")
#' )
#'
lightThreeJS <- function(builder,
                             color = "#FFFFFF"
){

  ##############
  # Validate parameters
  #################

  # Builder
  if( !( "JS_EVAL" %in% class(builder) ) ) stop("Light builder is not a javascript function")

  # Color
  tryCatch(is.matrix(col2rgb(color)),
           error = function(e) stop("Invalid color") )

  #Create object
  ret <- list(
    buildFunc = builder,
    color = color
  )

  #Asign class and return
  class(ret) <- LIGHTTHREEJS_CLASS

  return(ret)
}


#' @export
is.lightThreeJS <- function( obj ){
  if ( LIGHTTHREEJS_CLASS %in% class(obj)  ) return (TRUE)
  else if( is.list(obj)  )
    return(all(sapply(obj, function(x) LIGHTTHREEJS_CLASS %in% class(x) )))
  else
    return ( FALSE )
}

#' @export
toJSONList.lightThreeJS <- function( obj ){

  # Create parameter list (add color)
  params <- JSONThreeArgList(obj)

  # Create list
  ret <- list(
    buildFunc = obj$buildFunc,
    buildArgs = params
  )

  ret <- dropNullEmpty(ret)

  # Return list
  return(ret)
}

###################################################################
#                               LIGHTS
###################################################################

### CLASSES
AMBIENTLIGHT_CLASS <- "ambientLight"
DIRECTIONALLIGHT_CLASS <- "directionalLight"

# BUILDERS
light.builder <- list()
light.builder[[ AMBIENTLIGHT_CLASS  ]] <- htmlwidgets::JS("Composition3D.lights.ambientWrapper")
light.builder[[ DIRECTIONALLIGHT_CLASS  ]] <- htmlwidgets::JS("Composition3D.lights.directionalWrapper")


############ AMBIENT LIGHT ###############

#' @rdname light
#'
#' @details
#' \code{ambientLight} creates a  ambient light instance, that is, global uniform lighting
#'
#' @param color Material color (either a RGB string or a valid name)
#'
#' @export
ambientLight <- function( color = "#ffffff" ){

  # Validate input
  ret <- lightThreeJS( light.builder[[ AMBIENTLIGHT_CLASS  ]], color = color )

  # Append class
  class(ret) <- append(class(ret), AMBIENTLIGHT_CLASS)

  # Return
  return( ret )
}

JSONThreeArgList.ambientLight <- function(obj){
  # Return as an obj
  return( list(obj$color) )
}


################# DIRECTIONAL LIGHT ################

#' @rdname light
#'
#' @details \code{directionalLight} Creates a directional light instance with color, intensity and position parameters
#'
#' @param intensity 0-1 intensity value
#' @param from 3D numeric vector (x,y,z) (light direction)
#'
#' @export
directionalLight <- function( color = '#666666', intensity = 1 , from = c(10,0,10)){

  # Validate input
  if(!is.numeric(intensity) || intensity > 1 || intensity < 0 ) stop("Light intensity should be a numeric value between 0 and 1 ")
  if(!is.numeric(from) || length(from) != 3 ) stop("Vector from should be a 3D numeric vector")

  ret <- lightThreeJS( light.builder[[ DIRECTIONALLIGHT_CLASS  ]], color = color )

  # Add values
  ret$intensity = intensity
  ret$from = from

  # Append class
  class(ret) <- append(class(ret), DIRECTIONALLIGHT_CLASS)

  # Return
  return( ret )

}

JSONThreeArgList.directionalLight <- function(obj){
  # Return as an obj
  return( list(obj$color, obj$intensity, obj$from ) )
}
