OBJTHREEJS_CLASS <- "objThreeJS"

#' @rdname object
#' @aliases objThreeJS object
#' @title  ThreeJS 3DObject instance
#'
#' @description
#' A threeJS 3D Object definition. An object is defined by a geometry (see \link{geometry}) and
#' a surface material (see \link{material}). Opitionally you can add a object-specific helper
#'  (see \link{helper}) that will affect this object and all its descendants.
#'
#' @param geometries List of geoThreeJS to be included in this object (see \link{geometry}).
#' @param material Object material instance (see \link{material}).
#' @param name Optional name of the object. Doesnt need to be unique
#' @param helpers List of object helpers (see \link{helper}).
#' @param \dots Children 3D objects
#'
#' @examples
#' objThreeJS( sphereGeometry(1), lambertMaterial(color = "red"))
#' @export
objThreeJS <- function(geometries,
                        material,
                        name = NULL,
                        helpers  = NULL,
                        ...){

  ##############
  # Validate parameters
  #################

  # Material index
  if ( !is.geoThreeJS( geometries ) ) stop("Geometries are not geoThreeJS objects")

  # Vertex color func
  if ( !is.materialThreeJS(material) ) stop("Material is not a materialThreeJS object")

  # Name
  if ( !is.null(name) && !is.character(name) ) stop("Mesh name is not a string")

  # Helpers
  if ( !is.null(helpers) && !is.helperThreeJS(helpers) ) stop("Helpers should be helperTHREEjs")

  # Children
  if ( !is.objThreeJS(list(...))) stop("Children objects are not objThreeJS objects")

  if ( inherits(geometries, GEOTHREEJS_CLASS) ) geometries <- list(geometries)
  if ( inherits(material, MATERIALTHREEJS_CLASS) ) material <- list(material)
  if ( inherits(helpers, HELPERTHREEJS_CLASS) ) helpers <- list(helpers)

  # Check that helpers are mesh helpers
  if ( !is.null(helpers) )
    if ( any( vapply(helpers, function(x){x$type != "mesh"}, FUN.VALUE = logical(1) ) ) ) stop("Helpers should be mesh helpers")

  #Create object
  ret <- list(
    geometries = geometries,
    # If theres more than one material the JS side will create a MeshFaceMaterial that select which material to apply on a face based on the materialIndex value
    material = material,
    helpers = helpers,
    name = name,
    children = list(...)
  )

  #Asign class and return
  class(ret) <- OBJTHREEJS_CLASS

  return(ret)
}

#' @export
is.objThreeJS <- function(obj){
  if ( inherits(obj,OBJTHREEJS_CLASS)  ) return(TRUE)
  else if ( is.list(obj) )
    return(all(sapply(obj, inherits, OBJTHREEJS_CLASS )))
  else
    return(FALSE)
}

#' @export
toJSONList.objThreeJS <- function(obj){

  if ( isNullOrEmpty(obj$children) ) children <- NULL else children <- obj$children

  # Create list
  ret <- list(
   name = obj$name,
   geometries = lapply( obj$geometries, toJSONList ),
   material = lapply( obj$material, toJSONList ),
   helpers = lapply( obj$helpers, toJSONList),
   children = children
  )

  ret <- dropNullEmpty(ret)

  # Return list
  return(ret)
}
