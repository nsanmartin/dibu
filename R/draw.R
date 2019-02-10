
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
draw_lines.matrix <- function(data, in.order) {
    draw_lines(as.data.frame(data), in.order)
}

#' @export
draw_lines.data.frame <- function(data, in.order) {
    stopifnot(!is.list(in.order)) # must NOT be list
    data <- data[in.order,]
    lines(x = data$x, y = data$y)

}

write.point.labels <- function(data) {
    data <- as.data.frame(data)
    text(data$x, data$y, as.character(1:nrow(data)))

}

fst_cuad_move <- function(data, displacement) {
    UseMethod("fst_cuad_move")
}

fst_cuad_move.polyhedron <- function(data, displacement)
{
    displ <- get_displacement_for_drawing(data)
    data$vertices <- t(apply(data$vertices, 1, function(x) {
        x + displacement
    }))

    data
}

fst_cuad_move.list <- function(data, displacement) {

    fst_cuad <- list()
    for (i in 1:length(data)) {
        fst_cuad[[i]] <- t(apply(data[[i]], 1, function(x) {
            x + displacement
        }))
    }
    fst_cuad
}

fst_cuad_move.ensamble_list <- function(data, displacement) {

    res <- list()
    for (i in 1:length(data)) {
        elem <- list()
        for (j in 1:length(data[[i]])) {
            m <- data[[i]][[j]]
            elem[[j]] <-  t(apply(m, 1, function(x) {
                x + displacement
            }))

        }
        res[[i]] <- elem
    }
    class(res) <- c("ensamble_list", class(res))
    res
}

fst_cuad_move.matrix <- function(data,displacement) {
    stopifnot (dim(data)[2] == 3)
    fc <- apply(data, 1, function(x) x + displacement)
    t(fc)
}

get_displacement_for_drawing <- function(data) {
    UseMethod("get_displacement_for_drawing")
}

get_displacement_for_drawing.polyhedron <- function(data) {
    -apply(data$vertices, 2, min)
}

get_displacement_for_drawing.list <- function(data) {
    -apply(sapply(data, function(vertices) { apply(vertices, 2, min)}),
          1, min)
}

get_displacement_for_drawing.ensamble_list <- function(data) {
    apply(sapply(data, get_displacement_for_drawing), 1, max)
}

scale_for_drawing <- function(data) UseMethod("scale_for_drawing")

scale_for_drawing.polyhedron <- function(data) {
    data$vertices <- data$vertices/max(data$vertices)
    data
}
scale_for_drawing.list <- function(data) {
    scalar <- max(sapply(data, max))
    lapply(data, function(x) x / scalar)
}
scale_for_drawing.ensamble_list <- function(data) {
    scalar <- max(sapply(data, function(x) max(sapply(x, max))))
    res <- lapply(data, function(l) lapply(l, function(x) x / scalar))
    class(res) <- c("ensamble_list", class(res))
    res
}

#' @export
draw <- function(data, plane) UseMethod("draw")
#' @export
draw.polyhedron <- function(poly, plane) {
    poly <- project(poly, plane)

    displ <- get_displacement_for_drawing(poly)
    poly <- fst_cuad_move(poly, displ)
    poly <- scale_for_drawing(poly)
    
    sapply(poly$draw_order, function(x) {
        draw_lines(as.data.frame(poly$vertices), x)
    })
}
#' @export
draw.solid <- function(solid, plane) {
    matrix_list <- project(solid, plane)

    displ <- get_displacement_for_drawing(matrix_list)
    matrix_list <- fst_cuad_move(matrix_list, displ)
    matrix_list <- scale_for_drawing(matrix_list)

    for(i in 1:length(matrix_list)) {
        mat <- matrix_list[[i]]
        lapply(solid$draw_order, function(x) {
            draw_lines(mat, x)
        })
    }
}

#' @export
draw.ensamble <- function(ensamble, plane) {
    e_list <- project(ensamble, plane)
    
    displ <- get_displacement_for_drawing(e_list)
    e_list <- fst_cuad_move(e_list, displ)
    e_list <- scale_for_drawing(e_list)

    for(i in 1:length(e_list)) {
        l <- e_list[[i]]
        for (m in l) {
            lapply(ensamble$solids[[i]]$draw_order, function(x) {
                draw_lines(m, x)
            })
        }
    }

}



draw.ensamble.test <- function() {
    s1 <<- as.solid(rect_box(.2, .3, .25),
                     matrix(rep(c(0,.3),3), ncol=3))
    s2 <<- as.solid(rect_box(.1, .2, .25),
                    matrix(rep(c(.2,.7),3), ncol=3))
    p <<- orth_plane()
    en <<- ensamble(list(s1,s2))
    e. <<- project(en, p)

    dev.new(); plot.new()
    draw(en, p)

}

draw.solid.test <- function() {

    r <<- rect_box()
    offs <<- matrix(rep(c(0,.5), 3), ncol=3)
    p <<- orth_plane()
    s <<- as.solid(r, offs)
    dev.new(); plot.new()
    draw(s, p)
}

draw.poly.test <- function() {

    r <<- rect_box()
    p <<- orth_plane()
    dev.new(); plot.new()
    draw(r, p)
}
