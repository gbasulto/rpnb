predict.rpnb <- function(model, data, method='Simulated') {
  # method can be Simulated, Approximate, or Individual, or Ind_Mean
  # Individual method requires the dataframe to have the outcome observed in the dataframe
  # The Individual method uses a Bayesian approach to get individual coefficients and variance of the coefficients
  requireNamespace("MASS")
  requireNamespace("nlme")
  requireNamespace("randtoolbox")
  requireNamespace("maxLik")
  requireNamespace("dplyr")
  requireNamespace("stringr")

  rpar_formula <- model$rpar_formula
  formula <- model$formula
  mod1_frame <- stats::model.frame(formula, data)
  X_Fixed <- stats::model.matrix(formula, data)
  X_rand <- stats::model.matrix(rpar_formula, data)
  rpar <- all.vars(rpar_formula)

  num_vars_fixed <- length(all.vars(formula))
  num_vars_rand <- length(all.vars(rpar_formula))
  total_vars <- num_vars_fixed + num_vars_rand

  p <- as.array(model$estimate)

  pars <- head(p,total_vars)
  fixed_coefs <- head(pars, num_vars_fixed)
  random_coefs_means <- tail(pars, num_vars_rand)


  correlated <- FALSE #model$correlated
  alpha <- model$alpha
  scrambled <- model$scrambled
  ndraws <- model$numdraws

  if (correlated){
    Ch <- model$Cholesky
  }else{
    startrp <- length(p) - num_vars_rand
    endrp <- length(p) - 1
    rpar_sd <- as.vector(p[startrp:endrp])
  }

  nb_prob <- function(y, mu, alpha) {
    r <- 1/alpha
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

  if (correlated){
    scaled_draws <- hdraws %*% Ch
    draws <- scaled_draws
    for (i in 1:num_vars_rand){
      draws[,i] = scaled_draws[,i] + random_coefs_means[i]
    }
    draws <- t(draws)
  }
  else{
    draws <- hdraws
    for (i in 1:num_vars_rand){
      draws[,i] = draws[,i] * rpar_sd[i] + random_coefs_means[i]
    }
    draws <- t(draws)
  }

  mu_fixed <- exp(X_Fixed %*% fixed_coefs)


  xb_rand_mat <- apply(t(draws), 1, function(x) X_rand %*% x)


  rpar_mat <- exp(xb_rand_mat)
  pred_mat <- apply(rpar_mat, 2, function(x) x * mu_fixed)


  if (method == 'Approximate'){
    if (correlated){
      r_b <- log(rowMeans(exp(draws)))
    }else(r_b <- random_coefs_means + rpar_sd^2/2)

    r_b <- log(rowMeans(exp(draws)))

    mu_fixed <- exp(X_Fixed %*% fixed_coefs)

    mu_rand <- exp(X_rand %*% r_b)

    mu <- mu_fixed * mu_rand
    return(as.vector(mu))
  }
  else if (method=='Simulated'){
    mui = rowMeans(pred_mat)
    # hdr <- t(draws)
    # for (i in 1:ndraws){
    #   mu <- mu + exp(X_rand %*% hdr[i,]) * mu_fixed / ndraws
    # }

    return(mui)
  }
  else if (method=='Individual'){
    y <- model.response(mod1_frame)
    n_obs <- length(mu_fixed)
    ind_coefs <- matrix(0, nrow=n_obs, ncol=num_vars_rand)
    ind_var <- matrix(0, nrow=n_obs, ncol=num_vars_rand)
    ind_coefs_pred <- matrix(0, nrow=n_obs, ncol=num_vars_rand)
    prob_mat <- apply(pred_mat, 2, nb_prob, y=y, alpha=alpha)
    pred_i <- rep(0,n_obs)

    for (i in 1:num_vars_rand){
      hals <- draws[i,]

      b_i <- t(apply(prob_mat, 1, function(x) hals * x))
      bb_i <- t(apply(prob_mat, 1, function(x) hals * hals * x))


      bi <- rowSums(b_i)/rowSums(prob_mat)
      bbi <- rowSums(bb_i)/rowSums(prob_mat)

      var <- bbi - bi^2

      ind_coefs[,i] <- bi
      ind_var[,i] <- var
      ind_coefs_pred[,i] <- bi + var/2
    }

    xr <- exp(rowSums(ind_coefs_pred * X_rand))

    pred_i <- diag(outer(xr, as.vector(mu_fixed)))

    rp <- list(ind_coefs, ind_var, ind_coefs_pred, mu_fixed)


    oblist = list(pred_i, rp)
    return(pred_i)
  }
  else if (method == 'Ind_Mean'){
    y <- model.response(mod1_frame)
    n_obs <- length(mu_fixed)
    ind_coefs <- matrix(0, nrow=n_obs, ncol=num_vars_rand)
    ind_var <- matrix(0, nrow=n_obs, ncol=num_vars_rand)
    ind_coefs_pred <- matrix(0, nrow=n_obs, ncol=num_vars_rand)
    prob_mat <- apply(pred_mat, 2, nb_prob, y=y, alpha=alpha)
    pred_i <- rep(0,n_obs)

    for (i in 1:num_vars_rand){
      hals <- draws[i,]

      b_i <- t(apply(prob_mat, 1, function(x) hals * x))

      bi <- rowSums(b_i)/rowSums(prob_mat)

      ind_coefs[,i] <- bi
    }

    xr <- exp(rowSums(ind_coefs * X_rand))

    pred_i <- diag(outer(xr, as.vector(mu_fixed)))

    return(pred_i)
  }
  else{print('Please use one of the following methods: Approximate, Simulated, or Individual')}
}
