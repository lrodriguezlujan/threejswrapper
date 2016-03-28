MATERIALTHREEJS_CLASS <- "materialThreeJS"

#' @rdname material
#' @aliases material materialThreeJS lambertMaterial
#' @title ThreeJS surface material
#'
#' @description Object surface material definition.
#'
#' @param builder Javascript function that builds the geometry
#' @param params Unnamed list or vector with the arguments for the builder function
#'
#' @note Material constructors in threeJS admit an object as parameter and doesnt need to be rotated, translated, etc in any way.
#' Therefore, builder call in JS side is only func( params ) instead of Function.prototype.apply( func, params ). Please, consider
#' this if you add a new material.
#'
#' @examples
#' m <- lambertMaterial(color = "red")
materialThreeJS <- function( builder,
                             opacity = 1.0
                        ){

  ##############
  # Validate parameters
  #################

  # Builder
  if( !( "JS_EVAL" %in% class(builder) ) ) stop("Material builder is not a javascript function")

  # Opacity
  if( !is.numeric( opacity ) ) stop("Opacity should be a numeric value")
  else if( opacity > 1.0 || opacity < 0 ) warning("Opacity value cannot be greater than 1 or less than 0")

  # Fix opacity
  opacity <- max( min( opacity,1 ), 0)

  #Create object
  ret <- list(
    buildFunc = builder,
    opacity = opacity
  )

  #Asign class and return
  class(ret) <- MATERIALTHREEJS_CLASS

  return(ret)
}

#' @export
is.materialThreeJS <- function( obj ){
  if ( MATERIALTHREEJS_CLASS %in% class(obj)  ) return( TRUE )
  else if( is.list(obj) )
    return(all(sapply(obj, function(x) MATERIALTHREEJS_CLASS %in% class(x) )))
  else
    return ( FALSE )
}

#' @export
toJSONList.materialThreeJS <- function( obj ){

  # Create parameter list
  params <- JSONThreeArgList(obj)

  # Create list
  ret <- list(
    buildFunc = obj$buildFunc,
    buildArgs = params,
    opacity = obj$opacity,
    transparent = (obj$opacity < 1.0 ),
    visible = ( obj$opacity == 0 )
  )

  ret <- dropNullEmpty(ret)

  # Return list
  return(ret)
}

###################################################################
#                               MATERIALS
###################################################################

### CLASSES
LAMBERTMAT_CLASS <- "lambertMaterial"

# BUILDERS
material.builder <- list()
material.builder[[ LAMBERTMAT_CLASS  ]] <- htmlwidgets::JS("THREE.MeshLambertMaterial")


############ LAMBERT MATERIAL ###############

#' @rdname material
#'
#' @details
#' \code{lambertMaterial} Creates a lambert material instance
#'
#' @param color Material color (either a RGB string or a valid name)
#' @param fog Define whether the material color is affected by global fog settings
#' @param wireframe Render as wireframe
#' @param opacity Opacity value. See \link{materialThreeJS}
#' @param \dots Any valid argument for MeshLamberMaterial (\url{http://threejs.org/docs/#Reference/Materials/MeshLambertMaterial})
#'
#' @export
lambertMaterial <- function( color = "#ffffff" , fog = F, wireframe = F, opacity = 1.0, ...){

  # Validate input

  # Color
  tryCatch(is.matrix(col2rgb(color)),
           error = function(e) stop("Invalid color") )

  # Fog affect
  if( !is.logical(fog) ) stop("Fog affect flag is not boolean ")

  # Wireframe
  if( !is.logical(wireframe) ) stop("Wireframe render flag is not boolean ")

  # Create generic material
  ret <- materialThreeJS( material.builder[[ LAMBERTMAT_CLASS ]], opacity = opacity )

  # Set params
  ret$params <- c( list(color = color, fog = fog, wireframe = wireframe ), list(...) )

  # Append class
  class(ret) <- append(class(ret), LAMBERTMAT_CLASS)

  # Return
  return( ret )
}

#' @export
JSONThreeArgList.lambertMaterial <- function(obj){
  # Return as an obj
  return(obj$params)
}
