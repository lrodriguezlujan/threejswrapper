SCENETHREEJS_CLASS <- "sceneThreeJS"

#' @rdname scene
#'
#' @title S3 class for threeJS scene builder
#'
#' @description A threeJS scene definition. Includes different objects and lights
#'
#' @param objects 3D Objects to include in the scene \link{object}
#' @param lights List of \code{lightThreeJS} in the scene. See \link{light}
#' @param helpers List of \code{helperThreeJS} to be added to the scene. See \link{helper}
#' @param colorscale An \code{colorscaleThreeJS} object. See \link{colorscale}
#' @param objects.append If \code{FALSE} any object in a previous renderization is removed
#' @param lights.append If \code{FALSE} any light in a previous renderization is removed
#' @param camera.fov Camera field of view in degrees
#' @param camera.near Camera near plane distance
#' @param camera.far Camera far plane distance
#' @param camera.pos Camera position in cartesian coordinates
#' @param camera.lookat Initial target point of the camera in cartesian coordinates
#' @param controls Control names (TBD - Experimental)
#' @param renderer \code{auto} for automatic selection, \code{webgl} or \code{canvas}
#' @param bgColor Scene background color
#' @param animateFun JavaScript function \code{f(delta, composition)} to be called in each animation frame (EXPERIMENTAL)
#'
#' @examples
#' \dontrun{
#'  scene( list(objThreeJS( sphereGeometry(1), lambertMaterial(color = "red"), helpers = boundingBox() )),
#'         lights = list(
#'  directionalLight(
#'    "#FFFFFF",intensity = 0.5,from = c(1,1,0)
#'  ),
#'  directionalLight(
#'    "#FFFFFF",intensity = 0.5,from = c(0,0,1)
#'  ),
#'  ambientLight("#333333")
#'  ),
#'  helpers = list(gridHelper(),axisHelper(size = 10))
#'  )
#'
#' }
#' @export
sceneThreeJS <- function(objects,
                          lights = NULL ,
                          helpers = NULL,
                          colorscale = NULL,
                          objects.append = F,
                          lights.append = F,
                          camera.fov = NULL,
                          camera.near = NULL,
                          camera.far = NULL,
                          camera.pos = NULL,
                          camera.lookat = NULL,
                          controls = NULL,
                          renderer = c(NULL,"auto","webgl","canvas"),
                          bgColor = NULL,
                          animateFun = NULL #this.animationCb(delta, this);
  ){

  ##############
  # Validate parameters
  #################

  # Objects
  if ( !is.null(objects) && !is.objThreeJS(objects) ) stop("Scene object should be objThreeJS elements")

  # Light
  if ( !is.null(objects) && !is.lightThreeJS(lights) ) stop("Scene lingthing should be lightThreeJS elements")

  # Camera opts
  if ( !is.null(camera.fov) && !is.numeric(camera.fov) ) stop("Camera field of view should be a numeric value")
  if ( !is.null(camera.far) && !is.numeric(camera.far) ) stop("Camera far plane should be a numeric value")
  if ( !is.null(camera.near) && !is.numeric(camera.near) ) stop("Camera near plane should be a numeric value")
  if ( !is.null(camera.pos) && ((!is.numeric(camera.pos) ) || (length(camera.pos) != 3) ) ) stop("Camera position should be a 3D vector with its cartesian coordinates")
  if ( !is.null(camera.lookat) && ((!is.numeric(camera.lookat)) || (length(camera.lookat) != 3 ) ) ) stop("Camera lookat should be a 3D vector with its cartesian coordinates")

  # HELPERS
  if ( !is.null(helpers) && !is.helperThreeJS(helpers) ) stop("Helpers should be helperTHREEjs")

  # COLORSCALE
  if ( !is.null(colorscale) && !is.colorscaleThreeJS(colorscale) ) stop("Colorscale should be a colorscaleTHREEjs")

  # Control opts
  if ( !is.null(controls) && (!is.list(controls) ) ) stop("Control options should be a named list")

  # Color
  if (!is.null(bgColor))
    tryCatch(is.matrix(col2rgb(bgColor)),
           error = function(e) stop("Invalid color") )

  # renderer
  renderer <- match.arg(renderer)

  # controls
  controls <- match.arg(controls)

  # Animate fun
  if ( !is.null(animateFun) && (!("JS_EVAL" %in% class(animateFun) ) ) ) stop("Animation callback function should be a javascript function")

  if ( inherits(objects, OBJTHREEJS_CLASS)) objects <- list(objects)
  if ( inherits(lights, LIGHTTHREEJS_CLASS)) lights <- list(lights)
  if ( inherits(helpers, HELPERTHREEJS_CLASS) ) helpers <- list(helpers)

  #Create object
  ret <- list(
    objects = objects,
    objectsAppend = objects.append,
    lights = lights,
    lightsAppend = lights.append,
    helpers = helpers,
    colorscale = colorscale,
    controls = controls,
    camera = list(
      fov = camera.fov,
      far = camera.far,
      near = camera.near,
      pos = camera.pos,
      look = camera.lookat
    ),
    bgColor = bgColor,
    renderer = renderer,
    controls = controls,
    animateFun = animateFun
  )

  #Asign class and return
  class(ret) <- SCENETHREEJS_CLASS

  return(ret)
}

#' @rdname scene
#'
#' @param obj A scene object
#'
#' @export
is.sceneThreeJS <- function(obj){
  if ( is.list(obj) || is.vector(obj) )
    return(sapply(obj, function(x) SCENETHREEJS_CLASS %in% class(x) ))
  else
    return( SCENETHREEJS_CLASS %in% class(obj)  )
}

#' @rdname scene
#' @export
toJSONList.sceneThreeJS <- function(obj){

  # Create list
  ret <- list(
    objects = lapply(obj$objects, toJSONList ),
    objectsAppend = obj$objectsAppend,
    lights = lapply(obj$lights, toJSONList ),
    lightsAppend = obj$lightsAppend,
    helpers = lapply(obj$helpers, toJSONList),
    colorscale = toJSONList(obj$colorscale),
    controls = obj$controls,
    camera = obj$camera,
    bgColor = obj$bgColor,
    renderer = obj$renderer,
    animateFun = obj$animateFun
  )

  # Drop null or empty objects
  ret$camera <- dropNullEmpty(ret$camera)
  ret <- dropNullEmpty(ret)

  # Return list
  return(ret)
}


#' @rdname scene
#'
#' @description \code{scene} creates an HTML widgets that represents a \code{sceneThreeJS} object
#'
#' @param \dots (scene) Parameters for \code{sceneThreeJS}
#'
#' @importFrom htmlwidgets createWidget
#' @export
scene <- function(..., width = NULL, height = NULL ){
  ## Create scene
  scene <- sceneThreeJS(...);

  # create the widget
  htmlwidgets::createWidget("threejswrapper", toJSONList(scene) , width = width, height = height,
                            package = "threejswrapper" )
}

#' @rdname threejswrapper-shiny
#' @export
sceneOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(outputId, "threejswrapper", width, height, package = "threejswrapper")
}

#' @rdname threejswrapper-shiny
#' @export
renderScene <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, sceneOutput, env, quoted = TRUE)
}
