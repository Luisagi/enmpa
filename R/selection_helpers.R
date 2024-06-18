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




