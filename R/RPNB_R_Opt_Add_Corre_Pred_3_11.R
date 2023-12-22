# # The nb.rp function performs negative binomial regression with random parameters. The function takes as input the formula for the model, the formula for the random parameters, the dataset, the number of draws, a logical value indicating whether the random parameters are correlated, a logical value indicating whether to scramble the halton sequence used for generating random parameters, the method used for optimization and the maximum number of iterations for optimization. The function requires the 'MASS', 'nlme', 'randtoolbox', 'maxLik', 'dplyr' and 'stringr' packages.
# #
# # The function first extracts the model matrix for the fixed parameters, the model matrix for the random parameters, and the response variable. It checks if the model includes both fixed and random intercepts and if so, assumes the intercept is a random parameter. The function then creates a new formula for the negative binomial regression that includes both the fixed and random parameters. The model is fit using the glm.nb function in the MASS package, and the coefficients are extracted. The function then creates a starting vector for optimization, which includes the coefficients for the fixed parameters, the means of the random parameters, and the standard deviations of the random parameters (if they are uncorrelated) or the Cholesky decomposition matrix of the correlation matrix (if they are correlated).
# #
# # The function then defines two helper functions: one that calculates the probability of observing the response variable given the mean and dispersion parameter, and one that generates halton draws for the random parameters.
# #
# # The main function for optimization takes as input a vector of parameters, the response variable, the model matrix for the fixed parameters, the model matrix for the random parameters, the number of draws, a logical value indicating whether the random parameters are correlated, a logical value indicating whether to scramble the halton sequence used for generating random parameters, and the optimization method. The function extracts the coefficients for the fixed parameters, the means of the random parameters, and the dispersion parameter from the input vector of parameters. It then generates the random parameters using the halton_draws function and the means and standard deviations or Cholesky decomposition matrix. It calculates the negative log-likelihood for the negative binomial model using the nb_prob function and returns the sum of the negative log-likelihood and the penalty term for regularization.
# #
# # Finally, the main function optimizes the negative log-likelihood using the specified optimization method and returns the optimized parameter estimates.
#
#
# # The function nb.rp estimates a negative binomial model with random intercepts and slopes using maximum simulated likelihood. The function uses the MASS, nlme, randtoolbox, maxLik, dplyr, stringr, groupdata2, tibble, and cureplots packages.
# #
# # The input arguments of the function are:
# #
# # formula: a formula object specifying the model to be fitted.
# # rpar_formula: a formula object specifying the random intercepts and slopes.
# # data: a data frame containing the variables used in the model.
# # ndraws: the number of halton draws to use for the simulated maximum likelihood estimation (default is 1500).
# # scrambled: a logical indicating whether or not to scramble the halton sequence (default is FALSE).
# # correlated: a logical indicating whether or not the random intercepts and slopes are correlated (default is FALSE).
# # method: the optimization method to use for the maximum likelihood estimation (default is BHHH).
# # max.iters: the maximum number of iterations for the optimization method (default is 200).
# # The function first constructs the fixed and random design matrices and checks if both have an intercept. If both have an intercept, it assumes the intercept is a random parameter and removes it from the fixed formula and adds it to the random formula.
# #
# # The function then fits a negative binomial model using MASS::glm.nb(), obtains the model coefficients, and constructs the function for the negative binomial probability density function.
# #
# # Next, the function defines a function for generating halton draws. The number of parameters to be estimated is determined, and if the random formula contains only one parameter, correlated is set to FALSE. The function then generates halton draws and uses them to create the initial parameter estimates for the simulated maximum likelihood estimation.
# #
# # The function then defines the function that will be passed to the maxLik::maxLik() function for the simulated maximum likelihood estimation. This function takes the parameters to be estimated, the fixed and random design matrices, and the halton draws as input, and returns the negative log-likelihood.
# #
# # The function then calls maxLik::maxLik() to perform the simulated maximum likelihood estimation, passing the defined function and initial parameter estimates. Finally, the function returns the estimated model parameters and the negative log-likelihood.
#
#
#
#
#
# library("MASS")
# library("nlme")
# library("randtoolbox")
# library("maxLik")
# library("dplyr")
# library("stringr")
# library('groupdata2')
# library('tibble')
# library(cureplots)
#
# predict.rpnb <- function(model, data, method='Simulated') {
#   # method can be Simulated, Approximate, or Individual, or Ind_Mean
#   # Individual method requires the dataframe to have the outcome observed in the dataframe
#   # The Individual method uses a Bayesian approach to get individual coefficients and variance of the coefficients
#   requireNamespace("MASS")
#   requireNamespace("nlme")
#   requireNamespace("randtoolbox")
#   requireNamespace("maxLik")
#   requireNamespace("dplyr")
#   requireNamespace("stringr")
#
#   rpar_formula <- model$rpar_formula
#   formula <- model$formula
#   mod1_frame <- stats::model.frame(formula, data)
#   X_Fixed <- stats::model.matrix(formula, data)
#   X_rand <- stats::model.matrix(rpar_formula, data)
#   rpar <- all.vars(rpar_formula)
#
#   num_vars_fixed <- length(all.vars(formula))
#   num_vars_rand <- length(all.vars(rpar_formula))
#   total_vars <- num_vars_fixed + num_vars_rand
#
#   p <- as.array(model$estimate)
#
#   pars <- head(p,total_vars)
#   fixed_coefs <- head(pars, num_vars_fixed)
#   random_coefs_means <- tail(pars, num_vars_rand)
#
#
#   correlated <- FALSE #model$correlated
#   alpha <- model$alpha
#   scrambled <- model$scrambled
#   ndraws <- model$numdraws
#
#   if (correlated){
#     Ch <- model$Cholesky
#   }else{
#     startrp <- length(p) - num_vars_rand
#     endrp <- length(p) - 1
#     rpar_sd <- as.vector(p[startrp:endrp])
#   }
#
#   nb_prob <- function(y, mu, alpha) {
#     r <- 1/alpha
#     prob = dnbinom(y, size = r, mu = mu)
#     return(prob)
#   }
#
#   halton_draws <- function(ndraws, rpar, scrambled) {
#     requireNamespace("randtoolbox")
#     # returns a matrix of standard normal random variables based on halton draws
#
#     num_params <- length(rpar)
#     halton_seq <- randtoolbox::halton(ndraws, num_params, mixed = scrambled)
#     normal_haltons <- qnorm(halton_seq)
#     return(normal_haltons)
#   }
#
#   hdraws <- halton_draws(ndraws, rpar, scrambled)
#
#   if (correlated){
#     scaled_draws <- hdraws %*% Ch
#     draws <- scaled_draws
#     for (i in 1:num_vars_rand){
#       draws[,i] = scaled_draws[,i] + random_coefs_means[i]
#     }
#     draws <- t(draws)
#   }
#   else{
#     draws <- hdraws
#     for (i in 1:num_vars_rand){
#       draws[,i] = draws[,i] * rpar_sd[i] + random_coefs_means[i]
#     }
#     draws <- t(draws)
#   }
#
#   mu_fixed <- exp(X_Fixed %*% fixed_coefs)
#
#
#   xb_rand_mat <- apply(t(draws), 1, function(x) X_rand %*% x)
#
#
#   rpar_mat <- exp(xb_rand_mat)
#   pred_mat <- apply(rpar_mat, 2, function(x) x * mu_fixed)
#
#
#   if (method == 'Approximate'){
#     if (correlated){
#       r_b <- log(rowMeans(exp(draws)))
#     }else(r_b <- random_coefs_means + rpar_sd^2/2)
#
#     r_b <- log(rowMeans(exp(draws)))
#
#     mu_fixed <- exp(X_Fixed %*% fixed_coefs)
#
#     mu_rand <- exp(X_rand %*% r_b)
#
#     mu <- mu_fixed * mu_rand
#     return(as.vector(mu))
#   }
#   else if (method=='Simulated'){
#     mui = rowMeans(pred_mat)
#     # hdr <- t(draws)
#     # for (i in 1:ndraws){
#     #   mu <- mu + exp(X_rand %*% hdr[i,]) * mu_fixed / ndraws
#     # }
#
#     return(mui)
#   }
#   else if (method=='Individual'){
#     y <- model.response(mod1_frame)
#     n_obs <- length(mu_fixed)
#     ind_coefs <- matrix(0, nrow=n_obs, ncol=num_vars_rand)
#     ind_var <- matrix(0, nrow=n_obs, ncol=num_vars_rand)
#     ind_coefs_pred <- matrix(0, nrow=n_obs, ncol=num_vars_rand)
#     prob_mat <- apply(pred_mat, 2, nb_prob, y=y, alpha=alpha)
#     pred_i <- rep(0,n_obs)
#
#     for (i in 1:num_vars_rand){
#       hals <- draws[i,]
#
#       b_i <- t(apply(prob_mat, 1, function(x) hals * x))
#       bb_i <- t(apply(prob_mat, 1, function(x) hals * hals * x))
#
#
#       bi <- rowSums(b_i)/rowSums(prob_mat)
#       bbi <- rowSums(bb_i)/rowSums(prob_mat)
#
#       var <- bbi - bi^2
#
#       ind_coefs[,i] <- bi
#       ind_var[,i] <- var
#       ind_coefs_pred[,i] <- bi + var/2
#     }
#
#     xr <- exp(rowSums(ind_coefs_pred * X_rand))
#
#     pred_i <- diag(outer(xr, as.vector(mu_fixed)))
#
#     rp <- list(ind_coefs, ind_var, ind_coefs_pred, mu_fixed)
#
#
#     oblist = list(pred_i, rp)
#     return(pred_i)
#   }
#   else if (method == 'Ind_Mean'){
#     y <- model.response(mod1_frame)
#     n_obs <- length(mu_fixed)
#     ind_coefs <- matrix(0, nrow=n_obs, ncol=num_vars_rand)
#     ind_var <- matrix(0, nrow=n_obs, ncol=num_vars_rand)
#     ind_coefs_pred <- matrix(0, nrow=n_obs, ncol=num_vars_rand)
#     prob_mat <- apply(pred_mat, 2, nb_prob, y=y, alpha=alpha)
#     pred_i <- rep(0,n_obs)
#
#     for (i in 1:num_vars_rand){
#       hals <- draws[i,]
#
#       b_i <- t(apply(prob_mat, 1, function(x) hals * x))
#
#       bi <- rowSums(b_i)/rowSums(prob_mat)
#
#       ind_coefs[,i] <- bi
#     }
#
#     xr <- exp(rowSums(ind_coefs * X_rand))
#
#     pred_i <- diag(outer(xr, as.vector(mu_fixed)))
#
#     return(pred_i)
#   }
#   else{print('Please use one of the following methods: Approximate, Simulated, or Individual')}
# }
#
# crashes <- read.csv("C:/Users/jwood2/Box/RPNB/WA/WA_FW_Full.csv")
# crashes$ID <- as.factor(crashes$ID)
# crashes <- crashes[crashes$length>=0.1,]
#
# #make this example reproducible
# set.seed(1)
#
# # set 9 folds for k-fold cross validation, accounting for the ID of the location
# crashes <- fold(crashes, id_col = 'ID', k=9,  method = 'n_last', handle_existing_fold_cols = 'remove')
#
# nb.equat <- total_crash ~  Nlanes_5 + Nlanes_6 + Nlanes_7 +
#   Nlanes_8 + Nlanes_9 + LW_great_13 + RollingTerrain + lnaadt_per_lane + lnlength  +  hc_dens
#
# summary(glm.nb(nb.equat, crashes[crashes$.folds!=1,]))
#
# equat <- total_crash ~  lnaadt_per_lane + Nlanes_5 + Nlanes_6 + Nlanes_7 +
#                         Nlanes_8 + Nlanes_9 + LW_great_13 + RollingTerrain
# r_equat <- ~ 0 +  hc_dens + lnlength
#
# run.mod <- function(fold.no, correlated=FALSE){
#   rpnb.model.est <- nb.rp(equat,
#                       r_equat,
#                       data=crashes[crashes$.folds != fold.no,],
#                       ndraws=1500,
#                       scrambled = FALSE,
#                       correlated = correlated,
#                       method = 'BHHH',
#                       max.iters = 500)
#   return(rpnb.model.est)
# }
#
# get.preds <- function(rpmodel, fold.no){
#   df <- crashes[crashes$.folds == fold.no,]
#
#   df$app_pred <- predict.rpnb(rpmodel, df, method = 'Approximate')
#   df$sim_pred <- predict.rpnb(rpmodel, df, method = 'Simulated')
#
#   df$ind_pred <- predict.rpnb(rpmodel, df, method = 'Individual')
#   df$ind_pred_mean <- predict.rpnb(rpmodel, df, method = 'Ind_Mean')
#
#   attr(df$ind_pred, "groups") <- NULL
#
#   return(df)
# }
#
# run_cure <- function(outcome, prediction, variable){
#   residual <- outcome - prediction
#   cure_df <- calculate_cure_dataframe(variable, residual)
#   cure_plot(cure_df)
# }
#
#
# #library(yaml)
#
# ###################################################
# # Fold 1###########################################
# ###################################################
# mod1 <- run.mod(1, correlated = FALSE)
# summary(mod1)
#
#
# #write_yaml(mod1, "mod1.yml")
#
# df.pred.1 <- get.preds(mod1, 1)
#
# write.csv(df.pred.1, 'WA_fold_1_9.csv')
#
# # run_cure(outcome = df.pred.1$total_crash, prediction = df.pred.1$app_pred, variable = df.pred.1$lnaadt_per_lane)
# #
# # run_cure(outcome = df.pred.1$total_crash, prediction = df.pred.1$sim_pred, variable = df.pred.1$lnaadt_per_lane)
# #
# # run_cure(outcome = df.pred.1$total_crash, prediction = df.pred.1$ind_pred, variable = df.pred.1$lnaadt_per_lane)
# #
#
#
# ###################################################
# # Fold 2###########################################
# ###################################################
# foldno = 2
# mod2 <- run.mod(foldno, correlated = FALSE)
# summary(mod2)
#
# #write_yaml(mod2, "mod2.yml")
#
# df.pred.2 <- get.preds(mod2, foldno)
#
# write.csv(df.pred.2, 'WA_fold_2.csv')
#
# # run_cure(outcome = df.pred.2$total_crash, prediction = df.pred.2$app_pred, variable = df.pred.2$lnaadt_per_lane)
# #
# # run_cure(outcome = df.pred.2$total_crash, prediction = df.pred.2$sim_pred, variable = df.pred.2$lnaadt_per_lane)
# #
# # run_cure(outcome = df.pred.2$total_crash, prediction = df.pred.2$ind_pred, variable = df.pred.2$lnaadt_per_lane)
#
# ###################################################
# # Fold 3###########################################
# ###################################################
# foldno = 3
# mod3 <- run.mod(foldno, correlated = FALSE)
# summary(mod3)
# #write_yaml(mod3, "mod3.yml")
#
# df.pred.3 <- get.preds(mod3, foldno)
#
# write.csv(df.pred.3, 'WA_fold_3.csv')
#
# # run_cure(outcome = df.pred.3$total_crash, prediction = df.pred.3$app_pred, variable = df.pred.3$lnaadt_per_lane)
# #
# # run_cure(outcome = df.pred.3$total_crash, prediction = df.pred.3$sim_pred, variable = df.pred.3$lnaadt_per_lane)
# #
# # run_cure(outcome = df.pred.3$total_crash, prediction = df.pred.3$ind_pred, variable = df.pred.3$lnaadt_per_lane)
#
#
# ###################################################
# # Fold 4###########################################
# ###################################################
# foldno = 4
# mod4 <- run.mod(foldno, correlated = FALSE)
# summary(mod4)
# #write_yaml(mod4, "mod4.yml")
#
# df.pred.4 <- get.preds(mod4, foldno)
#
# write.csv(df.pred.4, 'WA_fold_4.csv')
#
# # run_cure(outcome = df.pred.4$total_crash, prediction = df.pred.4$app_pred, variable = df.pred.4$lnaadt_per_lane)
# #
# # run_cure(outcome = df.pred.4$total_crash, prediction = df.pred.4$sim_pred, variable = df.pred.4$lnaadt_per_lane)
# #
# # run_cure(outcome = df.pred.4$total_crash, prediction = df.pred.4$ind_pred, variable = df.pred.4$lnaadt_per_lane)
#
#
# ###################################################
# # Fold 5###########################################
# ###################################################
# foldno = 5
# mod5 <- run.mod(foldno, correlated = FALSE)
# summary(mod5)
# #write_yaml(mod5, "mod5.yml")
#
# df.pred.5 <- get.preds(mod5, foldno)
#
# write.csv(df.pred.5, 'WA_fold_5.csv')
# #
# # run_cure(outcome = df.pred.5$total_crash, prediction = df.pred.5$app_pred, variable = df.pred.5$lnaadt_per_lane)
# #
# # run_cure(outcome = df.pred.5$total_crash, prediction = df.pred.5$sim_pred, variable = df.pred.5$lnaadt_per_lane)
# #
# # run_cure(outcome = df.pred.5$total_crash, prediction = df.pred.5$ind_pred, variable = df.pred.5$lnaadt_per_lane)
#
# ###################################################
# # Fold 6###########################################
# ###################################################
# foldno = 6
# mod6 <- run.mod(foldno, correlated = FALSE)
# summary(mod6)
# #write_yaml(mod6, "mod6.yml")
#
# df.pred.6 <- get.preds(mod6, foldno)
#
# write.csv(df.pred.6, 'WA_fold_6.csv')
#
# # run_cure(outcome = df.pred.6$total_crash, prediction = df.pred.6$app_pred, variable = df.pred.6$lnaadt_per_lane)
# #
# # run_cure(outcome = df.pred.6$total_crash, prediction = df.pred.6$sim_pred, variable = df.pred.6$lnaadt_per_lane)
# #
# # run_cure(outcome = df.pred.6$total_crash, prediction = df.pred.6$ind_pred, variable = df.pred.6$lnaadt_per_lane)
#
# ###################################################
# # Fold 7###########################################
# ###################################################
# foldno = 7
# mod7 <- run.mod(foldno, correlated = FALSE)
# summary(mod7)
# #write_yaml(mod7, "mod7.yml")
#
# df.pred.7 <- get.preds(mod7, foldno)
#
# write.csv(df.pred.7, 'WA_fold_7.csv')
#
# # run_cure(outcome = df.pred.7$total_crash, prediction = df.pred.7$app_pred, variable = df.pred.7$lnaadt_per_lane)
# #
# # run_cure(outcome = df.pred.7$total_crash, prediction = df.pred.7$sim_pred, variable = df.pred.7$lnaadt_per_lane)
# #
# # run_cure(outcome = df.pred.7$total_crash, prediction = df.pred.7$ind_pred, variable = df.pred.7$lnaadt_per_lane)
#
# ###################################################
# # Fold 8###########################################
# ###################################################
# foldno = 8
# mod8 <- run.mod(foldno, correlated = FALSE)
# summary(mod8)
# #write_yaml(mod8, "mod8.yml")
#
# df.pred.8 <- get.preds(mod8, foldno)
#
# write.csv(df.pred.8, 'WA_fold_8.csv')
#
# # run_cure(outcome = df.pred.8$total_crash, prediction = df.pred.8$app_pred, variable = df.pred.8$lnaadt_per_lane)
# #
# # run_cure(outcome = df.pred.8$total_crash, prediction = df.pred.8$sim_pred, variable = df.pred.8$lnaadt_per_lane)
# #
# # run_cure(outcome = df.pred.8$total_crash, prediction = df.pred.8$ind_pred, variable = df.pred.8$lnaadt_per_lane)
#
# ###################################################
# # Fold 4###########################################
# ###################################################
# foldno = 9
# mod9 <- run.mod(foldno, correlated = FALSE)
# summary(mod9)
# #write_yaml(mod9, "mod9.yml")
#
# df.pred.9 <- get.preds(mod9, foldno)
#
# write.csv(df.pred.9, 'WA_fold_9.csv')
#
# # run_cure(outcome = df.pred.9$total_crash, prediction = df.pred.9$app_pred, variable = df.pred.9$lnaadt_per_lane)
# #
# # run_cure(outcome = df.pred.9$total_crash, prediction = df.pred.9$sim_pred, variable = df.pred.9$lnaadt_per_lane)
# #
# # run_cure(outcome = df.pred.9$total_crash, prediction = df.pred.9$ind_pred, variable = df.pred.9$lnaadt_per_lane)
#
# ###################################################
# # Fold 10###########################################
# ###################################################
# # foldno = 10
# # mod10 <- run.mod(foldno, correlated = FALSE)
# # summary(mod10)
# # #write_yaml(mod10, "mod10.yml")
# #
# # df.pred.10 <- get.preds(mod10, foldno)
# #
# # write.csv(df.pred.10, 'WA_fold_10.csv')
# #
# # run_cure(outcome = df.pred.10$total_crash, prediction = df.pred.10$app_pred, variable = df.pred.10$lnaadt_per_lane)
# #
# # run_cure(outcome = df.pred.10$total_crash, prediction = df.pred.10$sim_pred, variable = df.pred.10$lnaadt_per_lane)
# #
# # run_cure(outcome = df.pred.10$total_crash, prediction = df.pred.10$ind_pred, variable = df.pred.10$lnaadt_per_lane)
#
#
#
# # library(stargazer)
# #
# # stargazer(mod1, mod2,
# #           type = "html",  #we use html output to match our planned R Markdown output
# #           title = "WA Urban Freeway CV models")
# #
# #
# #
# library(stargazer)
# library(DescrTab2)
# options(print_format = "tex")
#
# keepvars <- c('total_crash', 'lnaadt_per_lane',
#               'lnlength',  'hc_dens', 'Nlanes_5', 'Nlanes_6', 'Nlanes_7',
#   'Nlanes_8', 'Nlanes_9', 'LW_great_13', 'RollingTerrain')
#
# dfVars <- crashes[keepvars]
#
# dfVars$Nlanes_5 <- as.factor(dfVars$Nlanes_5)
# dfVars$Nlanes_6 <- as.factor(dfVars$Nlanes_6)
# dfVars$Nlanes_7 <- as.factor(dfVars$Nlanes_7)
# dfVars$Nlanes_8 <- as.factor(dfVars$Nlanes_8)
# dfVars$Nlanes_9 <- as.factor(dfVars$Nlanes_9)
#
# dfVars$LW_great_13 <- as.factor(dfVars$LW_great_13)
# dfVars$RollingTerrain <- as.factor(dfVars$RollingTerrain)
#
#
# tab_ss <- CreateTableOne(
#   data = dfVars,
#   includeNA = FALSE,
#   test = FALSE
# )
#
# summary(tab_ss, digitd = 4)
#
# print(tab_ss$ContTable, digits=4)
#
#
# DescrStats <- descr(dfVars)
# print(DescrStats)
#
# library(tableone)
#
# catvars <- keepvars <- c('Nlanes_5', 'Nlanes_6', 'Nlanes_7',
#                          'Nlanes_8', 'Nlanes_9', 'LW_great_13', 'RollingTerrain')
#
# tab2 <- CreateTableOne(vars = keepvars, data = crashes, factorVars = catvars)
#
# stargazer(tab2,
#           summary=FALSE,
#           type='latex')
#
#
# contVars <- c('total_crash', 'lnaadt_per_lane',
#               'lnlength',  'hc_dens')
#
# dfVarsCont <- crashes[contVars]
#
#
# DescrStats2 <- descr(dfVarsCont,
#                      summary_stats_cont = list(N = DescrTab2:::.N, mean = DescrTab2:::.mean,
#                                                sd = DescrTab2:::.sd, min = DescrTab2:::.min,
#                                                max =DescrTab2:::.max))
# print(DescrStats2)
#
# stargazer(dfVarsCont,
#           summary=TRUE,
#           type='latex')
# stargazer(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10,
#           type = "html",  #we use html output to match our planned R Markdown output
#           title = "WA Urban Freeway CV models")
#
# library(sjPlot)
# library(sjmisc)
# library(sjlabelled)
#
# tab_model(mod1, mod2,  auto.label = FALSE, show.ci = FALSE)
#
# tab_model(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9,  auto.label = FALSE, show.ci = FALSE)
#
# sjt.lm(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9,  file='WA_CV_models.doc')
#
# library(texreg)
#
# texreg(list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9), booktabs = TRUE, dcolumn = TRUE, digits=4)
#
# texreg(list(mod6, mod7, mod8, mod9), booktabs = TRUE, dcolumn = TRUE, digits=4)
#
# sumstats <- function(df, mod){
#   coefs <- mod$estimate
#   se <- 1/sqrt(-1*diag(mod$hessian))
#   t <- coefs/se
#   p <- 2*(1-pnorm(abs(t)))
#
#   y <- df$total_crash
#   ind_resid <- y - df$ind_pred
#   sim_resid <- y - df$sim_pred
#   app_resid <- y - df$app_pred
#
#   ind_resid_mean <- y - df$ind_pred_mean
#
#   print('Individual Coefficient Predictions')
#   print(paste('Bias =', round(sum(ind_resid)*100/sum(y),4)))
#   print(paste('RMSE =', round(sqrt(mean(ind_resid^2)),4)))
#   print(paste('MAE =', round(mean(abs(ind_resid)),4)))
#   print("")
#
#   print('Individual Mean Coefficient Predictions')
#   print(paste('Bias =', round(sum(ind_resid_mean)*100/sum(y),4)))
#   print(paste('RMSE =', round(sqrt(mean(ind_resid_mean^2)),4)))
#   print(paste('MAE =', round(mean(abs(ind_resid_mean)),4)))
#   print("")
#
#   print('Simulation-Based Predictions')
#   print(paste('Bias =', round(sum(sim_resid)*100/sum(y),4)))
#   print(paste('RMSE =', round(sqrt(mean(sim_resid^2)),4)))
#   print(paste('MAE =', round(mean(abs(sim_resid)),4)))
#   print("")
#
#   print('Approximation-Based Predictions')
#   print(paste('Bias =', round(sum(app_resid)*100/sum(y),4)))
#   print(paste('RMSE =', round(sqrt(mean(app_resid^2)),4)))
#   print(paste('MAE =', round(mean(abs(app_resid)),4)))
#
#   return(c(coefs, se, t, p))
# }
#
#
# cumres <- function(df){
#   df_cr <- df
#   df_cr$ind_mean_resid <- df_cr$total_crash - df_cr$ind_pred_mean
#   df_cr$ind_resid <- df_cr$total_crash - df_cr$ind_pred
#   df_cr$sim_resid <- df_cr$total_crash - df_cr$sim_pred
#   df_cr$residuals <- df_cr$total_crash - df_cr$app_pred
#   df_cr$`AADT per lane` <- exp(df_cr$lnaadt_per_lane)
#   df_cr <- df_cr[order(df_cr$`AADT per lane`),]
#   df_cr$cumresid <- cumsum(df_cr$residuals)
#   df_cr$cumresid_sim <- cumsum(df_cr$sim_resid)
#   df_cr$cumresid_ind <- cumsum(df_cr$ind_resid)
#   df_cr$cumresid_ind_mean <- cumsum(df_cr$ind_mean_resid)
#   df_cr$res_sq <- df_cr$residuals^2
#   df_cr$sdi <- cumsum(df_cr$res_sq)^0.5
#   sd_n = df_cr$sdi[nrow(df_cr)]
#   df_cr$sd_cure <- df_cr$sdi*(1-df_cr$sdi^2/sd_n^2)^0.5
#   df_cr$upper <- 1.96*df_cr$sd_cure # 4.653382
#   df_cr$lower <- -1.96*df_cr$sd_cure
#   return(df_cr)
# }
#
#
# library(ggplot2)
# library(tidyr)
# # cure.plot <- function(df_cure, fold){
# #
# #   reference <- c('cumresid', 'cumresid_sim', 'cumresid_ind', 'cumresid_ind_mean', 'upper', 'lower')
# #
# #   df_cure_long <- pivot_longer(df_cure, cols = c(cumresid, cumresid_sim, cumresid_ind, cumresid_ind_mean, upper, lower), names_to = 'Method', values_to = 'CURE')
# #   df_cure_long <- df_cure_long[order(factor(df_cure_long$Method, levels = reference)),]
# #
# #   df_cure_long$Method <- ifelse(df_cure_long$Method=='cumresid', 'Approximate', df_cure_long$Method)
# #   df_cure_long$Method <- ifelse(df_cure_long$Method=='cumresid_sim', 'Simulated', df_cure_long$Method)
# #   df_cure_long$Method <- ifelse(df_cure_long$Method=='cumresid_ind', 'Individual Coefficients', df_cure_long$Method)
# #   df_cure_long$Method <- ifelse(df_cure_long$Method=='cumresid_ind_mean', 'Individual Coefficients Mean', df_cure_long$Method)
# #   df_cure_long$Method <- ifelse(df_cure_long$Method=='upper', '95% Confidence Interval', df_cure_long$Method)
# #   df_cure_lower <- df_cure_long[df_cure_long$Method=='lower',]
# #   df_cure_other <- df_cure_long[df_cure_long$Method!='lower',]
# #   df_cure_other$size <- ifelse(df_cure_other$Method == 'Approximate', 1, 0.5)
# #
# #   legend_vals <- c('Approximate', 'Simulated', 'Individual Coefficients', 'Individual Coefficients Mean', '95% Confidence Interval')
# #
# #   ggplot(df_cure_other, aes(x = `AADT per lane`, y = CURE, group = Method)) +
# #     geom_line(aes(linetype=Method, color=Method), size = df_cure_other$size) +
# #     scale_linetype_manual(values=c('longdash',"solid", "twodash", 'dashed', 'solid')) +
# #     scale_color_manual(values=c('grey47', 'grey54','black', 'blue','black')) +
# #     geom_line(data=df_cure_lower, aes(x = `AADT per lane`, y = CURE), color = 'grey47', linetype = 'longdash', size = 1) +
# #     labs(x = 'AADT per lane', y = "Cum. Residuals", title = paste('CURE Plot for Fold', fold)) +
# #     theme_light()
# #     #theme_light(legend.position="top")
# #
# # }
#
#
# cure.plot <- function(df_cure, fold){
#
#   reference <- c('cumresid', 'cumresid_sim', 'cumresid_ind', 'cumresid_ind_mean', 'upper', 'lower')
#
#   df_cure_long <- pivot_longer(df_cure, cols = c(cumresid, cumresid_sim, cumresid_ind, cumresid_ind_mean, upper, lower), names_to = 'Method', values_to = 'CURE')
#   df_cure_long <- df_cure_long[order(factor(df_cure_long$Method, levels = reference)),]
#
#   df_cure_long <- df_cure_long[!df_cure_long$Method=="cumresid_ind",]
#
#   df_cure_long$Method <- ifelse(df_cure_long$Method=='cumresid', 'Approximate', df_cure_long$Method)
#   df_cure_long$Method <- ifelse(df_cure_long$Method=='cumresid_sim', 'Simulated', df_cure_long$Method)
#   #df_cure_long$Method <- ifelse(df_cure_long$Method=='cumresid_ind', 'Individual Coefficients', df_cure_long$Method)
#   df_cure_long$Method <- ifelse(df_cure_long$Method=='cumresid_ind_mean', 'Individual Coefficients', df_cure_long$Method)
#   df_cure_long$Method <- ifelse(df_cure_long$Method=='upper', '95% Confidence Interval', df_cure_long$Method)
#   df_cure_lower <- df_cure_long[df_cure_long$Method=='lower',]
#   df_cure_other <- df_cure_long[df_cure_long$Method!='lower',]
#   df_cure_other$size <- ifelse(df_cure_other$Method == 'Approximate', 1, 0.5)
#
#   legend_vals <- c('Approximate', 'Simulated', 'Individual Coefficients', '95% Confidence Interval')
#
#   ggplot(df_cure_other, aes(x = AADT, y = CURE, group = Method)) +
#     geom_line(aes(linetype=Method, color=Method), size = df_cure_other$size) +
#     scale_linetype_manual(values=c('longdash',"solid",  'dashed', 'solid')) +
#     scale_color_manual(values=c('grey47', 'grey54','black', 'black')) +
#     geom_line(data=df_cure_lower, aes(x = AADT, y = CURE), color = 'grey47', linetype = 'longdash', size = 1) +
#     labs(x = 'AADT', y = "Cum. Residuals", title = paste('CURE Plot for Fold', fold)) +
#     theme_light(base_size = 25)
#   #theme_light(legend.position="top")
#
# }
#
# df_cure.1 <- cumres(df=df.pred.1)
# df_cure.2 <- cumres(df=df.pred.2)
# df_cure.3 <- cumres(df=df.pred.3)
# df_cure.4 <- cumres(df=df.pred.4)
# df_cure.5 <- cumres(df=df.pred.5)
# df_cure.6 <- cumres(df=df.pred.6)
# df_cure.7 <- cumres(df=df.pred.7)
# df_cure.8 <- cumres(df=df.pred.8)
# df_cure.9 <- cumres(df=df.pred.9)
# #df_cure.10 <- cumres(df=df.pred.10)
#
# mod1.sum <- sumstats(df.pred.1, mod1)
# mod2.sum <- sumstats(df.pred.2, mod2)
# mod3.sum <- sumstats(df.pred.3, mod3)
# mod4.sum <- sumstats(df.pred.4, mod4)
# mod5.sum <- sumstats(df.pred.5, mod5)
# mod6.sum <- sumstats(df.pred.6, mod6)
# mod7.sum <- sumstats(df.pred.7, mod7)
# mod8.sum <- sumstats(df.pred.8, mod8)
# mod9.sum <- sumstats(df.pred.9, mod9)
# #mod10.sum <- sumstats(df.pred.10, mod10)
#
#
# library(ggpubr)
#
# p1 <- cure.plot(df_cure.1, 1)
# p2 <- cure.plot(df_cure.2, 2)
# p3 <- cure.plot(df_cure.3, 3)
# p4 <- cure.plot(df_cure.4, 4)
# p5 <- cure.plot(df_cure.5, 5)
# p6 <- cure.plot(df_cure.6, 6)
# p7 <- cure.plot(df_cure.7, 7)
# p8 <- cure.plot(df_cure.8, 8)
# p9 <- cure.plot(df_cure.9, 9)
# #p10 <- cure.plot(df_cure.10, 10)
#
# ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol=3, nrow=3, common.legend = TRUE, legend="bottom")
#
#
# save(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, file = "WA_Models.RData")
#
#
