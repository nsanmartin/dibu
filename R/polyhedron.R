#' @export
as.polyhedron <- function(mat, draw_order) {
    stopifnot("matrix" %in% class(mat))
    stopifnot(ncol(mat) == 3)
    dimnames(mat) <- list(NULL, c("x", "y", "z"))
    structure (list(vertices=mat, draw_order=draw_order),
               class = c("polyhedron"))
}


#' @export
rect_box <- function(x = .1, y = .1, z = .1) {
    draw.order <- list(c(5,1:4,1),
                       c(4, 8:5, 8),
                       c(3, 7),
                       c(2,6))

    x <- c(0, 0, 1, 1, 0, 0, 1, 1) * x
    y <- c(0, 1, 1, 0, 0, 1, 1, 0) * y
    z <- c(0, 0, 0, 0, 1, 1, 1, 1) * z
    structure(list(vertices = matrix(c(x, y, z), ncol = 3,
                                     dimnames=list(NULL,
                                                   c("x", "y", "z"))),
                   draw_order = draw.order),
              class = c("polyhedron"))
}


new.solid <- function(poly, offsets) {
    stopifnot("polyhedron" %in% class(poly))
    stopifnot(ncol(offset) == 3)
    poly$offsets <- offsets
    class(poly) <- c("solid", class(poly))
    poly
}
