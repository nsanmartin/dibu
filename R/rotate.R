rotation_matrix_x <- function(degre, radians=FALSE) {
    if (!radians) {
        theta <- (degre %% 360) * pi/180
    } else {
        theta <- radians
    }
    
    matrix(c(cos(theta), -sin(theta), 0,
             sin(theta), cos(theta), 0,
             0, 0, 1), ncol=3, byrow = TRUE)
}


rotate_x <- function(x, theta) UseMethod("rotate_x")
rotate_x.numeric <- function(x, theta) {
    1
}

