# detection of response curves that are concave (non-unimodal)
detect_concave <- function(glm_coefficents) {
  if (missing(glm_coefficents)) {
    stop("Argumet 'glm_coefficents' must be defined.")
  }
  coef <- glm_coefficents
  varname <- names(coef)

  ccoef <- grep("\\^2", varname, value = TRUE)

  if (length(ccoef) == 0) {
    return("")
  } else {
    cccoef <- ccoef[coef[ccoef] >= 0]

    return(paste(cccoef, collapse = ", "))
  }
}



# contribution_formulas <- function(data, dependent, independent, weights = NULL,
#                                   type = "lqp", range = c(0.0001, 0.2),
#                                   by = 0.0001) {
#
#   f <- get_formulas(dependent = dependent, independent = independent,
#                     type = type, all_combinations = FALSE)
#
#   allfit <- glm(as.formula(f), data = data, family = binomial(link = "logit"),
#                 weights = weights)
#
#   av <- anova(allfit, test = "Chisq")
#   preds <- rownames(av)
#   devs <- av$Deviance[-1]
#
#   sequ <- seq(range[1], range[2], by = by)
#
#   norm_dev <- seq(0, max(devs), length.out = 100)
#
#   forms <- lapply(sequ, function(x) {
#     thres <- quantile(norm_dev, x)
#
#     pick <- devs > thres
#
#     pred <- preds[-1][pick]
#
#     paste(dependent, "~", paste(pred, collapse = " + "))
#   })
#
#   forms <- unique(unlist(forms))
#   forms <- forms[length(forms):1]
#
#   return(forms)
# }


# jackknife_selection <- function(dependent, independent, data, weights,
#                                 type = "lpq", aic_limit = 2){
#
#   ff <- get_formulas(dependent, independent, type = type, all_combinations = F)
#
#   aux <- gsub(paste0(dependent," ~ "), "", ff)
#   features <- unlist(strsplit(gsub(" ", "", aux), split = "[+]"))
#
#   comb <- utils::combn(features, length(features) - 1)
#
#   withon <- sapply(1:length(features), function(x) {
#     get_formulas(dependent, rev(features)[x], type = "l", all_combinations = F)
#   })
#
#   without <- sapply(1:length(features), function(x) {
#     get_formulas(dependent, comb[, x], type = "l", all_combinations = F)
#   })
#
#
#   metrics <- c("ROC_AUC", "TSS", "AIC")
#
#   full <- model_validation(ff, data = sdmdata, weights = myweights)[2, metrics]
#
#   withon <- lapply(withon, function(x) {
#     model_validation(x, data = sdmdata, weights = myweights)[2, metrics]
#   })
#
#   without <- lapply(without, function(x) {
#     model_validation(x, data = sdmdata, weights = myweights)[2, metrics]
#   })
#
#   withon <- do.call(rbind, withon)
#   without <- do.call(rbind, without)
#
#   rownames(withon) <- features
#   rownames(without) <- features
#
#   aucs <- rbind(withon$ROC_AUC, without$ROC_AUC)
#   tsss <- rbind(withon$TSS, without$TSS)
#   aics <- rbind(withon$AIC, without$AIC)
#
#   colnames(aucs) <- features
#   rownames(aucs) <- c("with_only", "without")
#
#   colnames(tsss) <- features
#   rownames(tsss) <- c("with_only", "without")
#
#   colnames(aics) <- features
#   rownames(aics) <- c("with_only", "without")
#
#   auc_nos <- aucs[2, ] > 0.5
#   auc_nos <- names(auc_nos)[auc_nos < 0]
#
#   tss_dif <- full$TSS - tsss[2, ]
#   tss_dif <- names(tss_dif)[tss_dif < 0]
#
#   aic_dif <- full$AIC - aics[2, ]
#   aic_dif <- names(aic_dif)[aic_dif > aic_limit]
#
#   to_reduce <- union(union(auc_nos, tss_dif), aic_dif)
#   to_reduce1 <- intersect(union(auc_nos, tss_dif), aic_dif)
#
#   to_keep <- features[!features %in% to_reduce]
#   to_keep1 <- features[!features %in% to_reduce1]
#
#   return(list(
#     full_model = full,
#     without = without,
#     with_only = withon,
#     detrimental_auc = auc_nos,
#     detrimental_tss = tss_dif,
#     detrimental_aic = aic_dif,
#     detrimental_union = to_reduce,
#     detrimental_intersection = to_reduce1,
#     to_keep_union = to_keep,
#     to_keep_intersection = to_keep1))
#
# }



