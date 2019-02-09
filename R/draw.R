#' @export
cube <- function(n) {
    draw.order <- list(c(5,1:4,1),
                       c(4, 8:5, 8),
                       c(3, 7),
                       c(2,6))

    x <- c(0, 0, 1, 1, 0, 0, 1, 1)
    y <- c(0, 1, 1, 0, 0, 1, 1, 0)
    z <- c(0, 0, 0, 0, 1, 1, 1, 1)
    structure(list(vertices = matrix(c(x, y, z), ncol = 3,
                                     dimnames=list(NULL,
                                                   c("x", "y", "z"))),
                   draw_order = draw.order),
              class = c("polyhedron"))
}

#' @export
draw_points<- function(data) UseMethod("draw_points")
#' @export
draw_points.matrix <- function(data) { draw(as.data.frame(data)) }
#' @export
draw_points.data.frame <- function(data) {
    points(x = data$x, y = data$y)
}



#' @export
draw_lines <- function(data, in.order) UseMethod("draw_lines")


#' @export
draw_lines.martix <- function(data, in.order) {
    draw_lines(as.data.frame(data), in.order)
}

#' @export
draw_lines.data.frame <- function(data, in.order) {
    stopifnot(!is.list(in.order))
    data <- data[in.order,]
    lines(x = data$x, y = data$y)

}

write.point.labels <- function(data) {
    data <- as.data.frame(data)
    text(data$x, data$y, as.character(1:nrow(data)))

}

fst_cuad_move_scale <- function(data) {
    stopifnot (dim(data)[2] == 3) 
    fc <- apply(data, 1, function(x) x - apply(data, 2, min))
    t(fc / max(fc))
}

#' @export
draw <- function(data) UseMethod("draw")
#' @export
draw.polyhedron <- function(poly) {
    poly$vertices <- fst_cuad_move_scale(poly$vertices)
    sapply(poly$draw_order, function(x) {
        draw_lines(as.data.frame(poly$vertices), x)
    })
}
