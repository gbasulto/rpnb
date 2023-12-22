#' Negative Binomial Regression with Random Parameters
#'
#' The function nb.rp estimates a negative binomial model with random intercepts
#' and slopes using maximum simulated likelihood. The function uses the MASS,
#' nlme, randtoolbox, maxLik, dplyr, stringr, groupdata2, tibble, and cureplots
#' packages.
#'
#' The function first constructs the fixed and random design matrices and checks
#' if both have an intercept. If both have an intercept, it assumes the
#' intercept is a random parameter and removes it from the fixed formula and
#' adds it to the random formula.
#'
#' The function then fits a negative binomial model using MASS::glm.nb(),
#' obtains the model coefficients, and constructs the function for the negative
#' binomial probability density function.
#'
#' Next, the function defines a function for generating halton draws. The number
#' of parameters to be estimated is determined, and if the random formula
#' contains only one parameter, correlated is set to FALSE. The function then
#' generates halton draws and uses them to create the initial parameter
#' estimates for the simulated maximum likelihood estimation.
#'
#' The function then defines the function that will be passed to the
#' maxLik::maxLik() function for the simulated maximum likelihood estimation.
#' This function takes the parameters to be estimated, the fixed and random
#' design matrices, and the halton draws as input, and returns the negative
#' log-likelihood.
#'
#' The function then calls maxLik::maxLik() to perform the simulated maximum
#' likelihood estimation, passing the defined function and initial parameter
#' estimates. Finally, the function returns the estimated model parameters and
#' the negative log-likelihood.
#'
#' ------------------------------------------------------------------
#'
#' The function takes as input the formula for the model, the formula for the
#' random parameters, the dataset, the number of draws, a logical value
#' indicating whether the random parameters are correlated, a logical value
#' indicating whether to scramble the halton sequence used for generating random
#' parameters, the method used for optimization and the maximum number of
#' iterations for optimization. The function requires the 'MASS', 'nlme',
#' 'randtoolbox', 'maxLik', 'dplyr' and 'stringr' packages.
#'
#' The function first extracts the model matrix for the fixed parameters, the
#' model matrix for the random parameters, and the response variable. It checks
#' if the model includes both fixed and random intercepts and if so, assumes the
#' intercept is a random parameter. The function then creates a new formula for
#' the negative binomial regression that includes both the fixed and random
#' parameters. The model is fit using the glm.nb function in the MASS package,
#' and the coefficients are extracted. The function then creates a starting
#' vector for optimization, which includes the coefficients for the fixed
#' parameters, the means of the random parameters, and the standard deviations
#' of the random parameters (if they are uncorrelated) or the Cholesky
#' decomposition matrix of the correlation matrix (if they are correlated).
#'
#' The function then defines two helper functions: one that calculates the
#' probability of observing the response variable given the mean and dispersion
#' parameter, and one that generates halton draws for the random parameters.
#'
#' The main function for optimization takes as input a vector of parameters, the
#' response variable, the model matrix for the fixed parameters, the model
#' matrix for the random parameters, the number of draws, a logical value
#' indicating whether the random parameters are correlated, a logical value
#' indicating whether to scramble the halton sequence used for generating random
#' parameters, and the optimization method. The function extracts the
#' coefficients for the fixed parameters, the means of the random parameters,
#' and the dispersion parameter from the input vector of parameters. It then
#' generates the random parameters using the halton_draws function and the means
#' and standard deviations or Cholesky decomposition matrix. It calculates the
#' negative log-likelihood for the negative binomial model using the nb_prob
#' function and returns the sum of the negative log-likelihood and the penalty
#' term for regularization.
#'
#' Finally, the main function optimizes the negative log-likelihood using the
#' specified optimization method and returns the optimized parameter estimates.
#'
#' @param formula Formula object specifying the model to be fitted.
#' @param rpar_formula Formula object specifying the random intercepts and
#'   slopes.
#' @param data Data frame containing the variables used in the model
#' @param ndraws Number of Halton draws to use for the simulated maximum
#'   likelihood estimation (default is 1500).
#' @param scrambled Logical indicating whether or not to scramble the halton
#'   sequence (default is FALSE).
#' @param correlated Logical indicating whether or not the random intercepts and
#'   slopes are correlated (default is FALSE).
#' @param method Optimization method to use for the maximum likelihood
#'   estimation (default is BHHH).
#' @param max_iters Maximum number of iterations for the optimization method
#'   (default is 200).
#'
#' @return Sum of the negative log-likelihood and the penalty term for
#'   regularization.
#' @export
#'
#' @examples
#' NULL
nb.rp <- function(
    formula,
    rpar_formula,
    data,
    ndraws = 1500,
    scrambled = FALSE,
    correlated = FALSE,
    method = 'BHHH',
    max_iters = 200) {

  ## Define formula for random parameters
  rpars <- rpar_formula[[2]]
  n_rpar <- length(rpars)
  if (n_rpar == 1) {
    rpar_formula <-
      formula(paste0("param ~ ", rpars, " + (", rpars, "|subject)"))
  }

  mod1_frame <- stats::model.frame(formula, data)
  X_Fixed <- stats::model.matrix(formula, data)
  X_rand <- stats::model.matrix(rpar_formula, data)
  y_name <- all.vars(formula)[1]
  y <- model.response(mod1_frame)

  nb_vars <- c(all.vars(formula)[-1], all.vars(rpar_formula))
  nb_formula <- reformulate(nb_vars, response = y_name, intercept = TRUE)

  # Check to ensure both random and fixed parameters do not both have an
  # intercept
  has_intercept <- function(X_Fixed){
    return(stringr::str_detect(colnames(X_Fixed), regex("intercept", ignore_case = TRUE)))
  }

  if ((sum(has_intercept(X_Fixed)) + sum(has_intercept(X_rand))) > 1) {
    message('You can only include an intercept as a fixed parameter OR a random parameter. \n Assuming intercept is a random parameter. \n Use "- 1" in the formula that does not have the intercept to specify a formula without an intercept.')

    formula <- stats::update(formula, . ~ . - 1)
    rpar_formula <- stats::update(rpar_formula, ~ . + 1)

    X_rand <- stats::model.matrix(rpar_formula, data)
    X_Fixed <- stats::model.matrix(formula, data)

  }

  nb_model <- MASS::glm.nb(nb_formula, data)
  params <- coef(nb_model)

  x_fixed_names <- colnames(X_Fixed)
  x_rand_names <- rpar <- colnames(X_rand)

  if (length(rpar) < 2){
    correlated = FALSE
  }

  nb_prob <- function(y, mu, alpha) {
    r <- 1 / alpha
    prob = dnbinom(y, size = r, mu = mu)
    return(prob)
  }


  halton_draws <- function(ndraws, rpar, scrambled) {
    requireNamespace("randtoolbox")
    # returns a matrix of standard normal random variables based on halton draws

    num_params <- length(rpar)
    halton_seq <- randtoolbox::halton(ndraws, num_params, mixed = scrambled)
    normal_haltons <- qnorm(halton_seq)
    return(normal_haltons)
  }

  hdraws <- halton_draws(ndraws, rpar, scrambled)
  Lfixed <- length(x_fixed_names)
  Lparams <- length(params)
  Lrpar = length(rpar)

  if ('intercept' %in% rpar){
    start <- params[2:Lfixed+1]
    start <- append(start, params[1])
    start <- append(start, params[Lfixed+2:Lparams])
  } else{
    start <- params
  }
  randparam_means = tail(start, Lrpar)

  if (correlated){
    rparam_var <- abs(randparam_means)/2
    rparam_var <- diag(rparam_var)
    Chl <- chol(rparam_var)

    for (i in 1:length(rpar)){
      for (j in 1:length(rpar)){
        if (i >= j){
          start <- append(start, Chl[j,i])
        }
      }
    }
  }else{
    for (i in 1:length(rpar)){
      start <- append(start, 0.1)
    }
  }

  start <- append(start, log(0.1)) # initial log of overdispersion paramater
  start <- unlist(start, recursive = TRUE, use.names = FALSE)


  p_nb_rp <- function(p, y, X_Fixed, X_rand, hdraws, rpar, correlated, est_method){
    N_fixed = length(x_fixed_names)
    N_rand = length(x_rand_names)

    coefs <- as.array(p)
    fixed_coefs <- head(coefs,N_fixed)
    h <- head(coefs, N_fixed + N_rand)
    random_coefs_means <- tail(h, N_rand)
    alpha <- exp(tail(coefs,1))

    t <- tail(coefs, length(coefs)-N_rand-N_fixed)



    if (length(rpar)>1){
      if (correlated){ # Generate correlated random draws
        chol_vals <- head(t,-1)
        Ch <- matrix(0, N_rand, N_rand)
        counter = 1
        for (i in 1:N_rand){
          for (j in 1:N_rand){
            if (j<=i){
              Ch[j,i] <- chol_vals[counter]
              counter <- counter + 1
            }
          }
        }
        scaled_draws <- t(hdraws %*% Ch)
        draws <- apply(scaled_draws, 2, function(x) x + random_coefs_means)

      }else{
        rand_sdevs <- head(t,-1)
        scaled_draws <- apply(hdraws, 1, function(x) x * rand_sdevs)

        draws <- apply(scaled_draws, 2, function(x) x + random_coefs_means)
      }
    } else{
      scaled_draws <- hdraws * rand_sdevs
      draws <- scaled_draws + random_coefs_means
    }

    mu_fixed <- exp(X_Fixed %*% fixed_coefs)

    xb_rand_mat <- apply(t(draws), 1, function(x) X_rand %*% x)

    rpar_mat <- exp(xb_rand_mat)
    pred_mat <- apply(rpar_mat, 2, function(x) x * mu_fixed)

    prob_mat <- apply(pred_mat, 2, nb_prob, y=y, alpha=alpha)

    probs <- rowSums(prob_mat)/ndraws

    ll <- sum(log(probs))
    if (est_method == 'bhhh' || method == 'BHHH'){
      return(log(probs))
    } else{return(ll)}
  }

  fit <- maxLik::maxLik(p_nb_rp,
                        start = start,
                        y = y,
                        X_Fixed = X_Fixed,
                        X_rand = X_rand,
                        hdraws = hdraws,
                        rpar = rpar,
                        correlated = correlated,
                        est_method = method,
                        method = method,
                        control = list(iterlim = max_iters, printLevel = 2))

  print(summary(fit))
  x_rand_names <- paste(x_rand_names, ':Mean')
  x_names = c(x_fixed_names, x_rand_names)

  N_fixed = length(x_fixed_names)
  N_rand = length(x_rand_names)

  coefs <- as.array(fit$estimate)

  t <- tail(coefs, length(coefs)-N_rand-N_fixed)


  if (correlated){
    for (i in rpar){
      for (j in rpar){
        x_names <- append(x_names, paste('Cholesky Value for' ,paste(j, i, sep=":")))
      }
    }
    chol_vals <- head(t,-1)
    fit$Cholesky <- matrix(0, N_rand, N_rand)
    counter = 1
    for (i in 1:N_rand){
      for (j in 1:N_rand){
        if (j<=i){
          fit$Cholesky[j,i] <- chol_vals[counter]
          counter <- counter + 1
        }
      }
    }

    fit$Covariance <- t(fit$Cholesky) %*% fit$Cholesky
    fit$Correlation <- cov2cor(fit$Covariance)
  }
  else{
    for (i in rpar){
      x_names <- append(x_names, paste('St. Dev.', i))
    }
  }



  x_names <- append(x_names, 'ln(alpha)')
  names(fit$estimate) <- x_names

  fit$formula <- formula
  fit$rpar_formula <- rpar_formula
  fit$alpha <- exp(tail(fit$estimate,1))
  fit$scrambled <- scrambled
  fit$numdraws <- ndraws
  fit$correlated <- correlated

  return(fit)
}
