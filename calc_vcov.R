calc_vcov <- function(y, X, beta, stratum_vector) {
  # X      : n x p design matrix (no stratum intercepts)
  # beta   : length-p coefficient vector
  # strata : length-n vector defining conditioning strata
  
  # validation
  stopifnot(
    is.matrix(X),
    length(y) == nrow(X),
    length(beta) == ncol(X),
    length(stratum_vector) == nrow(X)
  )
  
  # Recode strata as consecutive integers 1, ..., S
  n_strata <- length(unique(stratum_vector))
  stratum_vector <- as.numeric(factor(stratum_vector, labels = 1:n_strata))
  
  # Initialize the Fisher information matrix for beta
  # Dimension: p x p, where p = number of regression coefficients
  # We will accumulate information across strata
  p <- ncol(X)
  I <- matrix(0, p, p)
  
  # Loop over strata
  # Each iteration adds the Fisher information contribution
  # from one multinomial likelihood (one conditional Poisson stratum)
  for (s in seq_len(n_strata)) {
    
    # Identify which observations belong to stratum s
    # These observations share a fixed total count
    idx <- which(stratum_vector == s)
    
    # Skip degenerate strata
    if (length(idx) <= 1) next
    
    # Extract the design matrix rows for stratum s
    # Dimension: n_s x p
    # drop = FALSE ensures Xs stays a matrix even if n_s = 1
    Xs <- X[idx, , drop = FALSE]
    
    # Compute the linear predictor for stratum s
    # eta_is = x_is^T beta
    eta <- as.vector(Xs %*% beta)
    
    # Convert linear predictors to unnormalized intensities
    # These are proportional to multinomial probabilities
    ps <- exp(eta)
    
    # Normalize to obtain multinomial probabilities
    # p_is = exp(eta_is) / sum_j exp(eta_js)
    # These probabilities sum to 1 within each stratum
    ps <- ps / sum(ps)
    
    # Compute the observed total count in stratum s
    # This is the conditioning value in the conditional Poisson
    Ns <- sum(y[idx])
    
    # Construct the multinomial covariance (weight) matrix
    # diag(ps) gives Var(Y_is | Ns)
    # tcrossprod(ps) = ps %*% t(ps) gives Cov(Y_is, Y_js | Ns)
    #
    # Ws = diag(ps) - ps ps^T
    #
    # This matrix:
    # - encodes negative correlation within strata
    # - has rank (n_s - 1)
    # - removes one degree of freedom due to conditioning
    Ws <- diag(ps) - tcrossprod(ps)
    
    # Add the Fisher information contribution from stratum s
    #
    # Multinomial Fisher information:
    # I_s = Ns * X_s^T Ws X_s
    #
    # Ns scales the information by the total count in the stratum
    I <- I + Ns * crossprod(Xs, Ws %*% Xs)
  }
  
  # Invert the total Fisher information matrix
  # This yields the variance–covariance matrix of beta
  # under the conditional Poisson (multinomial) likelihood
  return(ginv(I))
}