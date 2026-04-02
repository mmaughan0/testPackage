beta_hat <- function(X, y) {
  # Estimate beta using the normal equation
  # beta_hat = (X^T X)^(-1) X^T y
  XtX <- t(X) %*% X
  XtX_inv <- solve(XtX)
  XtY <- t(X) %*% y
  beta_hat <- XtX_inv %*% XtY
  return(beta_hat)
}

##generating data and then testing against lm() function
set.seed(123)
n <- 100    
p <- 5
X <- matrix(rnorm(n * p), nrow = n, ncol = p)   
beta_true <- c(1, 2, 3, 4, 5)
y <- X %*% beta_true + rnorm(n) 
beta_estimated <- beta_hat(X, y)
print(beta_estimated)   
lm_model <- lm(y ~ X - 1)
print(coef(lm_model))   

##3.5 creating my_theme() function
##usethis::use_package("ggplot2")
my_theme <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Comic Sans MS", size = 12),
      axis.title = ggplot2::element_text(face = "bold"),
      panel.grid.major = ggplot2::element_line(color = "gray80"),
      panel.grid.minor = ggplot2::element_blank()
    )
}

