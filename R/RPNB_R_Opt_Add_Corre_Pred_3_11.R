#
#
# # Load packages -----------------------------------------------------------
#
# library(MASS)
# library(nlme)
# library(randtoolbox)
# library(maxLik)
# library(dplyr)
# library(stringr)
# library(groupdata2)
# library(tibble)
# library(cureplots)
#
#
# # Load data ---------------------------------------------------------------
#
# ## tmp path
# data_path <-
#   "C:/Users/basulto/Box/R Files for Packages/RPNB/WA_Urban_Freeways.RData"
# crashes <- load(data_path)
#
# crashes <- read.csv("C:/Users/jwood2/Box/RPNB/WA/WA_FW_Full.csv")
# crashes$ID <- as.factor(crashes$ID)
# crashes <- crashes[crashes$length >= 0.1, ]
#
# #make this example reproducible
# set.seed(1)
#
# # set 9 folds for k-fold cross validation, accounting for the ID of the location
# crashes <- fold(crashes, id_col = 'ID', k = 9,  method = 'n_last', handle_existing_fold_cols = 'remove')
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
