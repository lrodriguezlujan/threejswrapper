GEOTHREEJS_CLASS <- "geoThreeJS"

#' @rdname geometry
#' @aliases geometry geoThreeJS sphere cylinder mesh torus parametric
#'
#' @title ThreeJS Geometries
#'
#' @description A generic threeJS geometry definition. The geometry of an object defines its shape.
#'
#' @param builder Javascript function that builds the geometry
#' @param materialIndex Material index to apply to the geometry faces.
#' @param vertex.color.fun Javascript function \code{f( vertex , geo, index, extra )} that computes the color of each vertex. Should return a RGB color.
#' @param face.color.fun  Javascript function \code{f( face, vertex a, vertex b, vertex c , geo, index, extra )} that computes the color of each face. Should return a RGB color.
#' @param face.material.fun Javascript function \code{f(face, vertex a, vertex b, vertex c , geo, index, extra )} that computes the material of each face. Should return a integer.
#' @param compute.bbox If true, \code{.computeBoundingBox()} function is called on the geometry.
#' @param compute.fn  If true, \code{.computeFaceNormals()} function is called on the geometry. Required for light effects!
#' @param compute.vn If true, \code{.computeFaceNormals()} function is called on the geometry.
#' @param remove.dupvn If true, \code{.mergeVertices()} function is called on the geometry and duplicated vertices are merged.
#' @param color.extra Extra parameter for color functions
#'
#' @return A geoThreeJS object
geoThreeJS <- function( builder,
                        materialIndex = 0,
                        vertex.color.fun = NULL,
                        face.color.fun = NULL,
                        face.material.fun = NULL,
                        compute.bbox = F,
                        compute.fn = F,
                        compute.vn = F,
                        remove.dupvn = T,
                        color.extra = NULL ){

  ##############
  # Validate parameters
  #################

  # Builder
  if ( !( "JS_EVAL" %in% class(builder) ) ) stop("Geometry builder is not a javascript function")

  # Material index
  if ( !is.numeric( materialIndex ) || floor(materialIndex) != materialIndex ) stop("Material index should be an integer value")

  # Vertex color func
  if ( !is.null(vertex.color.fun) && ( "JS_EVAL" %in% vertex.color.fun ) ) stop("Vertex color function is not a javascript function")

  # Face color func
  if ( !is.null(face.color.fun) && ( "JS_EVAL" %in% face.color.fun ) ) stop("Face color function is not a javascript function")

  # Face material func
  if ( !is.null(face.material.fun) && ( "JS_EVAL" %in% face.material.fun ) ) stop("Face material function is not a javascript function")

  # BBOX
  if ( !is.logical(compute.bbox) ) stop( "Compute bounding box flag is not boolean")

  # Face normal
  if ( !is.logical(compute.fn) ) stop( "Compute face normals flag is not boolean")

  # Vertex normal
  if ( !is.logical(compute.vn) ) stop( "Compute vertex normals flag is not boolean")

  # Remove duplicated vertices
  if ( !is.logical(remove.dupvn) ) stop( "Remove duplciated vertex flag is not boolean")

  #Create object
  ret <- list(
    buildFunc = builder,
    materialIndex = materialIndex,
    vcFun = vertex.color.fun,
    fcFun = face.color.fun,
    fmFun = face.material.fun,
    bbox = compute.bbox,
    fn = compute.fn,
    vn = compute.vn,
    dupvn = remove.dupvn,
    extra = color.extra
  )

  #Asign class and return
  class(ret) <- GEOTHREEJS_CLASS

  return(ret)
}

#' @rdname geometry
#'
#' @export
is.geoThreeJS <- function(obj) {
  if ( inherits(obj, GEOTHREEJS_CLASS) ) return(TRUE)
  else if ( is.list(obj) )
    return(all(sapply(obj, function(x) GEOTHREEJS_CLASS %in% class(x) )))
  else
    return( FALSE )
}


#' @rdname geometry
#'
#' @export
toJSONList.geoThreeJS <- function(obj) {

  # Create parameter list
  params <- JSONThreeArgList(obj)

  # Create list
  ret <- list(
    buildFunc = obj$buildFunc,
    buildArgs = params,
    materialIndex = obj$materialIndex,
    vcFun = obj$vcFun,
    fcFun = obj$fcFun,
    fmFun = obj$fmFun,
    bbox = obj$bbox,
    fn = obj$fn,
    vn = obj$vn,
    dupvn = obj$dupvn,
    extra = obj$extra
  )

  ret <- dropNullEmpty(ret)

  # Return list
  return(ret)
}

###################################################################
#                               GEOMETRIES
###################################################################

### CLASSES
MESHGEOMETRY_CLASS <- "meshGeometry"
SPHEREGEOMETRY_CLASS <- "sphereGeometry"
CYLINDERGEOMETRY_CLASS <- "cylinderGeometry"
PARAMETRICGEOMETRY_CLASS <- "parametricGeometry"
TORUSGEOMETRY_CLASS <- "torusGeometry"

# BUILDERS
geo.builder <- list()
geo.builder[[ MESHGEOMETRY_CLASS  ]] <- htmlwidgets::JS("Composition3D.geometries.meshWrapper")
geo.builder[[ SPHEREGEOMETRY_CLASS  ]] <- htmlwidgets::JS("Composition3D.geometries.sphereWrapper")
geo.builder[[ CYLINDERGEOMETRY_CLASS  ]] <- htmlwidgets::JS("Composition3D.geometries.cylinderWrapper")
geo.builder[[ PARAMETRICGEOMETRY_CLASS  ]] <- htmlwidgets::JS("Composition3D.geometries.parametricWrapper")
geo.builder[[ TORUSGEOMETRY_CLASS  ]] <- htmlwidgets::JS("Composition3D.geometries.torusWrapper")


#################### MESH GEOMETRY ################################

#' @rdname geometry
#'
#' @details
#' \code{meshGeometry} Creates a generic mesh object defined by a set of vertices and the face indexes.
#' The vertex color array could be also defined in the last column of the vertices matrix, but then face normals need to be computed.
#'
#' @param vertices A numeric matrix or dataframe where the first three columns are the cartesian coordiantes of the vertices. The fourth could be used as the vertex color.
#' @param faces A numeric matrix or dataframe where the first three columns are the vertex index of each triangular face of the mesh.
#'
#' @note If indexes in faces do not start at 0, they will be fixed.
#'
#' @examples
#' \dontrun{
#'    runShinyExample("mesh")
#' }
#'
#' @export
meshGeometry <- function(vertices, faces, ... ){

  # Validate input

  # Vertices
  if( is.null(vertices) || ( !is.matrix(vertices) && !is.data.frame(vertices) ) || !is.numeric(vertices[, 1:3 ])  )
    stop("Vertex set should be a numeric matrix/df with xyz coordinates of each vertex")

  if( ncol(vertices) > 4 ) warning( "Additional columns in vertex set will be ignored" )

  # Vertex colors
  if( ncol(vertices) > 3 )
    if( !all(sapply(vertices[,4], function(X) {
      tryCatch(is.matrix(col2rgb(X)),
               error = function(e) FALSE) }) ) ) stop("Invalid vertex colors")

  # Faces
  if( is.null(faces) || ( !is.matrix(faces) && !is.data.frame(faces) ) || !is.numeric(faces[, 1:3 ])  )
    stop("Faces should be a numeric matrix/df with indexes of each triangular face")

  if( ncol(faces) > 3 ) warning( "Additional columns in faces will be ignored" )

  # Create geo base class
  ret <- geoThreeJS( geo.builder[[ MESHGEOMETRY_CLASS ]], ... )

  # Fix face indexes ( JS arrays start at 0 )
  idx_range <- range( as.vector(faces) )

  # If max(idx_range) matches with nrow(vertices) - 1 then we dont move any index
  if( idx_range[2] != ( nrow(vertices) - 1 ) ){

    # Move faces to start at 0
    if( idx_range[1] > 0 )
      faces <- faces - idx_range[1]
  }

  # Add additional params
  ret$vertexSet <- as.data.frame(vertices[, 1:3 ])
  colnames(ret$vertexSet) <- c("x","y","z")
  ret$faceSet <- as.data.frame(faces)
  colnames(ret$faceSet) <- c("a","b","c")

  # Set vertices. The fourth column of vertices (if exists) will be used as vertex color
  if( ncol( vertices ) == 4 )
    ret$vertexColors <- vertices[,4]
  else
    ret$vertexColors <- NULL

  # Append class
  class(ret) <- append(class(ret), MESHGEOMETRY_CLASS)

  # Return
  return( ret )
}

JSONThreeArgList.meshGeometry <- function(obj) {
  return(
    list(
      obj$vertexSet,
      obj$faceSet,
      obj$vertexColors
    )
  )
}

########################################## END: MESH GEOMETRY ##########################

#################### SPHERE GEOMETRY ################################

#' @rdname geometry
#'
#' @details
#' \code{sphereGeometry} Creates a sphere geometry with a given radius among other parameters.
#' User can also give center position and rotation to apply
#'
#' @param radius  Sphere radius
#' @param position Sphere center coordinates
#' @param rotation Sphere x,y,z rotation
#' @param wsegs  Number of horizontal segments to draw the sphere
#' @param hsegs Number of vertical segments to draw the sphere
#' @param phiStart  Initial phi angle (azimuth)
#' @param phiLen  Azimuth length (\code{2*pi} for a complete circumference)
#' @param thetaStart Initial theta angle (elevation)
#' @param thetaEnd Elevation length (\code{pi} for a complete circumference)
#' @param \dots Additional parameters for \code{\link{geoThreeJS}}.
#'
#' @examples
#' \dontrun{
#'    s <- sphereGeometry(1, position = c(1,1,1))
#'    runShinyExample("spheres")
#' }
#'
#' @export
sphereGeometry <- function( radius , position = c(0,0,0), rotation = c(0,0,0),
                          wsegs = 8, hsegs = 6,
                          phiStart = 0 , phiLen = 2*pi,
                          thetaStart = 0, thetaLen = pi
                          , ... ){

  # Validate input
  if( !is.numeric(radius) || length(radius)!=1  || radius <= 0 ) stop( "Radius should be a positive number")

  if( !is.numeric(position) || length(position)!=3  ) stop( "Position should be a 3D vector")

  if( !is.numeric(rotation) || length(rotation)!=3  ) stop( "Rotation should be a 3D vector")

  if( !is.numeric(wsegs) || ( floor(wsegs) != wsegs ) || wsegs <= 0 ) stop( "Wsegs should be a positive integer" )

  if( !is.numeric(hsegs) || ( floor(hsegs) != hsegs ) || hsegs <= 0 ) stop( "Hsegs should be a positive integer" )

  if( !is.numeric(phiStart) ||  phiStart < 0 || phiStart > 2*pi ) stop( "Phistart should be a value between 0,2pi" )

  if( !is.numeric(phiLen) ||  phiLen <= 0 || phiLen > 2*pi ) stop( "PhiLen should be a value between 0,2pi" )

  if( !is.numeric(thetaStart) ||  thetaStart < 0 || thetaStart > pi ) stop( "ThetaStart should be a value between 0,2pi" )

  if( !is.numeric(thetaLen) ||  thetaLen <= 0 || thetaLen > pi ) stop( "thetaLen should be a value between 0,2pi" )

  # Create base geometry
  ret <- geoThreeJS( geo.builder[[ SPHEREGEOMETRY_CLASS ]], ... )

  # Add parameters to structure
  ret$radius <- radius
  ret$position <- position
  ret$rotation <- rotation
  ret$wsegs <- wsegs
  ret$hsegs <- hsegs
  ret$phiStart <- phiStart
  ret$phiLen <- phiLen
  ret$thetaStart <- thetaStart
  ret$thetaLen <- thetaLen

  # Append class
  class(ret) <- append(class(ret), SPHEREGEOMETRY_CLASS)

  return( ret )

}

JSONThreeArgList.sphereGeometry <- function(obj) {
  return(
    list(
      # Add parameters to structure
      obj$radius,
      obj$position,
      obj$rotation,
      obj$wsegs,
      obj$hsegs,
      obj$phiStart,
      obj$phiLen,
      obj$thetaStart,
      obj$thetaLen
    )
  )
}

########################################## END: SPHERE GEOMETRY ##########################

#################### CYLINDER GEOMETRY ################################

#' @rdname geometry
#'
#' @details
#' \code{cylinderGeometry} Creates a cylinder geometry with given radius, position and orientation among other options.
#'
#' @param initPoint Cylinder starting point
#' @param endPoint Cylinder end point
#' @param height Cylinder height
#' @param orientation Cylinder orientation
#' @param initRad Cylinder initial radius
#' @param endRad Cylinder end radius (will increase/decrease linearly)
#' @param rSegs Number of radial segments used to draw the cylinder section (circle)
#' @param hSegs Number of height segments used to draw the cylinder
#' @param openEnded If \code{TRUE} the cylinder is draw without ends (hollow)
#'
#' @examples
#' \dontrun{
#' cyl <- cylinderGeometry(
#'  initPoint = c(0,0,0), height = 1, orientation = c(0,0,1),
#'  initRad = 1, endRad = 1)
#'  runShinyExample("cylinders")
#' }
#'
#' @export
cylinderGeometry <- function( initPoint = c(0,-50, 0), endPoint = NULL, height = 100, orientation = c(0,1,0),
                            initRad = 20, endRad = 20,
                            rSegs = 20, hSegs = 20,
                            openEnded = F,
                            thetaStart = 0, thetaLen = 2*pi
                            , ... ){

  # Validate input

  # Points
  if( !is.numeric(initPoint) || length(initPoint)!=3  ) stop( "InitPoint should be a 3D vector")
  if( !is.null(endPoint) && ( !is.numeric(endPoint) || length(endPoint)!=3 )  ) stop( "EndPoint should be a 3D vector")

  # height
  if( !is.null(height) && ( !is.numeric(height) || length(height)!=1  || height <= 0 ) ) stop( "Height should be a positive number")

  # Orientation
  if( !is.null(orientation) && ( !is.numeric(orientation) || length(orientation)!=3 )  ) stop( "Orientation should be a 3D vector")

  # Check that we have endPoint or height + orientation
  if( is.null(endPoint) && ( is.null(height) || is.null(orientation) )) stop( "Either endPoint or Orientation + height are required")

  # Rads
  if( !is.numeric(initRad) || length(initRad)!=1  || initRad <= 0 ) stop( "InitRad should be a positive number")
  if( !is.numeric(endRad) || length(endRad)!=1  || endRad <= 0 ) stop( "InitRad should be a positive number")

  # Segs
  if( !is.numeric(rSegs) || ( floor(rSegs) != rSegs ) || rSegs <= 0 ) stop( "rSegs should be a positive integer" )
  if( !is.numeric(hSegs) || ( floor(hSegs) != hSegs ) || hSegs <= 0 ) stop( "hSegs should be a positive integer" )

  # Theta
  if( !is.numeric(thetaStart) ||  thetaStart < 0 || thetaStart > 2*pi ) stop( "ThetaStart should be a value between 0,2pi" )
  if( !is.numeric(thetaLen) ||  thetaLen <= 0 || thetaLen > 2*pi ) stop( "thetaLen should be a value between 0,2pi" )

  ### END VALIDATION

  ## Compute Orientation + height if endPoint is given
  if(!is.null(endPoint)){

    #Get orientation vector
    orientation <- endPoint - initPoint

    #Get height (orientation length)
    height <- sqrt(sum(orientation^2))

    # Normalize orientation
    orientation <- orientation/height
  }
  else{
    # Normalize orientation (not really needed since JS also normalizes)
    orientation <- orientation/sqrt(sum(orientation^2))
  }

  # Create base geometry
  ret <- geoThreeJS( geo.builder[[ CYLINDERGEOMETRY_CLASS ]], ... )

  # Add parameters to structure
  ret$initP <- initPoint
  ret$height <- height
  ret$orientation <- orientation
  ret$initRad <- initRad
  ret$endRad <- endRad
  ret$rSegs <- rSegs
  ret$hSegs <- hSegs
  ret$open <- openEnded
  ret$thetaStart <- thetaStart
  ret$thetaLen <- thetaLen

  # Append class
  class(ret) <- append(class(ret), CYLINDERGEOMETRY_CLASS)

  return( ret )

}


JSONThreeArgList.cylinderGeometry <- function(obj) {
  return(
    list(
      # Add parameters to structure
      obj$initP,
      obj$height,
      obj$orientation,
      obj$initRad,
      obj$endRad,
      obj$rSegs,
      obj$hSegs,
      obj$open,
      obj$thetaStart,
      obj$thetaLen
    )
  )
}

########################################## END: CYLINDER GEOMETRY ##########################


#################### PARAMETRIC GEOMETRY ################################

#' @rdname geometry
#'
#' @details \code{parametricGeometry} Creates a parametric surface geometry from given JS function.
#' User can also set position, rotation, and scale.
#'
#' @param function JS function \code{f(x,y} that defines the parametric surface
#' @param nx Number of x-axis points to use to compute the parametric surface
#' @param ny Number of x-axis points to use to compute the parametric surface
#'
#' @examples
#' \dontrun{
#' geos <- parametricGeometry(
#' htmlwidgets::JS(
#' "function(u,v){
#'            u -= 0.5;
#'            u *= 2;
#'            v *= (2 * 3.1415);
#'            return( new THREE.Vector3( u * Math.cos(v) , u * Math.sin(v) , u ) ); }"
#'  ),
#'  vertex.color.fun = htmlwidgets::JS("function(v,g,i,e,cs){ return(cs.getColor( v.z )) }")
#'  )
#'  runShinyExample("parametric")
#' }
#'
#' @export
parametricGeometry <- function(fun, translation = c(0,0,0), rotation = c(0,0,0), scale = c(1,1,1), nx = 100, ny = 100, ... ){

  # Validate input

  # Function
  if( is.null(fun) || !inherits(fun , "JS_EVAL") ) stop( "Fun  should be a javascript function(u,v) -> vector ")

  # Position
  if( !is.numeric(translation) || length(translation)!=3  ) stop( "Position should be a 3D vector")

  # Rotation
  if( !is.numeric(rotation) || length(rotation)!=3  ) stop( "Rotation should be a 3D vector")

  # Rotation
  if( !is.numeric(scale) || length(scale)!=3 || any(scale<=0)  ) stop( "Rotation should be a positive 3D vector")

  # number of evaluations
  if( !is.numeric(nx) || ( floor(nx) != nx ) || nx <= 0 ) stop( "nx should be a positive integer" )
  if( !is.numeric(ny) || ( floor(ny) != ny ) || ny <= 0 ) stop( "ny should be a positive integer" )

  ### END VALIDATION


  # Create base geometry
  ret <- geoThreeJS( geo.builder[[ PARAMETRICGEOMETRY_CLASS ]], ... )

  # Add parameters to structure
  ret$fun <- fun
  ret$translation <- translation
  ret$rotation <- rotation
  ret$scale <- scale
  ret$nx <- nx
  ret$ny <- ny

  # Append class
  class(ret) <- append(class(ret), PARAMETRICGEOMETRY_CLASS)

  return( ret )

}

JSONThreeArgList.parametricGeometry <- function( obj ){
  return(
    list(
      obj$fun,
      obj$translation,
      obj$rotation,
      obj$scale,
      obj$nx,
      obj$ny
    )
  )
}

########################################## END: PARAMETRIC GEOMETRY ##########################


#################### TORUS GEOMETRY ################################

#' @rdname geometry
#'
#' @details \code{torusGeometry} Creates a torus geometry with given properties
#'
#' @param inner Torus inner radius
#' @param outer Torus outer radius
#'
#' @examples
#' \dontrun{
#'  torusGeometry(inner = 1, outer = 5, position = c(0,0,0), rotation = c(1,0,1))
#'  runShinyExample("torus")
#' }
#' @export
torusGeometry <- function(inner = 8, outer = 12, rsegs = 16, lsegs = 50, angle = 2*pi, position = c(0,0,0), rotation = c(0,0,0), ... ){

  # Validate input

  # Position
  if ( !is.numeric(position) || length(position) !=  3  ) stop( "Position should be a 3D vector")

  # Rotation
  if ( !is.numeric(rotation) || length(rotation) != 3  ) stop( "Rotation should be a 3D vector")
  # Angle
  if ( !is.numeric(angle) || length(angle) != 1 || angle > 2*pi || angle < 0  ) stop( "Angle should be a number between 0 and 2*pi")

  # number of segments
  if ( !is.numeric(rsegs) || (floor(rsegs) != rsegs ) || rsegs <= 0 ) stop( "rsegs should be a positive integer" )
  if ( !is.numeric(lsegs) || (floor(lsegs) != lsegs ) || lsegs <= 0 ) stop( "lsegs should be a positive integer" )

  # Outer radius
  if ( !is.numeric(outer) || length(outer) != 1 || outer < 0  ) stop( "outer radius should be a positive number")

  # Inner
  if ( !is.numeric(inner) || length(inner) != 1 || inner < 0  ) stop( "inner radius should be a positive number")

  # Check outer > inner
  if ( outer < inner ) stop("")

  ### END VALIDATION


  # Create base geometry
  ret <- geoThreeJS( geo.builder[[ TORUSGEOMETRY_CLASS ]], ... )

  # Add parameters to structure
  ret$tube <- (outer - inner)/2
  ret$radius <- (inner + ret$tube )
  ret$rsegs <- rsegs
  ret$lsegs <- lsegs
  ret$angle <- angle
  ret$position <- position
  ret$rotation <- rotation

  # Append class
  class(ret) <- append(class(ret), TORUSGEOMETRY_CLASS)

  return( ret )

}

JSONThreeArgList.torusGeometry <- function(obj){
  return(
    list(
      obj$radius,
      obj$tube,
      obj$rsegs,
      obj$lsegs,
      obj$angle,
      obj$position,
      obj$rotation
    )
  )
}

########################################## END: TORUS GEOMETRY ##########################
