#' Estimate beta using the normal equation
#'
#' @param X Design matrix
#' @param y Outcome vector
#' @return Estimated coefficient vector
#' @export
#'
#' @examples
#' X <- matrix(c(1, 2, 3, 4), ncol = 2)
#' y <- c(1, 2)
#' beta_hat(X, y)
beta_hat <- function(X, y) {
  XtX <- t(X) %*% X
  XtX_inv <- solve(XtX)
  XtY <- t(X) %*% y
  beta_hat <- XtX_inv %*% XtY
  return(beta_hat)
}

#' A custom ggplot theme
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' my_theme()
my_theme <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Comic Sans MS", size = 12),
      axis.title = ggplot2::element_text(face = "bold"),
      panel.grid.major = ggplot2::element_line(color = "gray80"),
      panel.grid.minor = ggplot2::element_blank()
    )
}