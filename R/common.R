#' Transform the object to a list
#'
#' Transform the given object to a list suitable for toJSON conversion
#' by any conventional package such as RJSONIO or jsonlite
#'
#' @title toJSONList
#' @param obj The original object
#'
#' @rdname toJSONList
#' @export toJSONList
toJSONList <- function(obj) {
  if (is.null(obj)) return(NULL)
  UseMethod("toJSONList", obj)
}

#' Creates the argument vector for a threeJS builder function
#'
#' Creates an unnamed list with the arguments needed for the builder func
#'
#' @title JSONThreeArgList
#' @param obj The original object
#'
#' @export
JSONThreeArgList <- function(obj ) {
  if (is.null(obj)) return(NULL)
  UseMethod("JSONThreeArgList", obj)
}

#' Auxiliar functions
isNullOrEmpty <- function(x) {
  return( is.null(x) || length(x) == 0)
}
dropNullEmpty <- function(x) {
  x[!vapply(x, isNullOrEmpty , FUN.VALUE = logical(1))]
}
