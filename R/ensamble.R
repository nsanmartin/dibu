#' @export
ensamble <- function(solids) {
    structure(list(solids = solids),
             class = "ensamble")
}

as.ensamble <- function(x) UseMethod("as.ensamble")

