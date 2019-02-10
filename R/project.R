#' @export
orth_plane <- function(normal = c(.1,.1,1)) {
    stopifnot(length(normal) == 3)
    v1 <- vector()
    v2 <- vector()
    if (normal[1] == 0 && normal[2] == 0) {
        v1 <- c(1,0,0)
        v2 <- c(0,1,0)
    } else if (normal[1] == 0) {
        v1 <- c(1, 0, 0)
        v2 <- c(0, -normal[3] / normal[2], 1) 
    } else {
        v1 <- c( -normal[2] / normal[1], 1, 0)
        v2 <- c( -normal[3] / normal[1], 0, 1)
    }
    plane <- matrix(c(v1, v2), nrow = 2, byrow = TRUE)
    qr.decom <- qr(t(plane))
    plane <- abs(cbind(qr.Q(qr.decom), normal / sqrt(sum(normal^2))))
    plane <- matrix(plane, nrow = 3,
                    dimnames = list(c("x", "y", "z"),
                                    c("", "", "normal")))

    structure(list(vectors = plane),
              class = "plane")
}

#' @export
project <- function(x, plane) UseMethod("project")

#' @export
project.numeric <- function(x, plane) {
    stopifnot("plane" %in% class(plane))
    stopifnot(length(x) == 3)
    normal <- plane$vectors[, "normal"]
    x - as.numeric(normal %*% x) * normal
}
#' @export
project.polyhedron <- function(data, plane) {
    data$vertices <- t(apply(data$vertices, 1, function (x) {
        project(x, plane)}))
    data
}
 
#' @export
project.solid <- function(data, plane) {
    res <- list()
    for (i in 1:nrow(data$offsets)) {
        res[[i]] <-  t(apply(data$vertices, 1, function (x) {
            project(x + data$offset[i,], plane)
        }))
    }
    res
}

#' @export
project.ensamble <- function(data, plane) {
    res <- list()
    for (s in data$solids) {
        res <- append(res, list(project(s, plane)))
    }
    class(res) <- c("ensamble_list", class(res))
    res
}
