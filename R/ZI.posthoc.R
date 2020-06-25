#' Produce a post-hoc analysed data.frame for your fit. 
#'  
#' @param fit The fit for which you'd like to produce a posthoc analysis. Don't forget to first run an Anova on it. 
#' @param factors The factors you'd like to check contrasts for. Give a vector containing all factors. 
#' @param nr.factors = 2 By default: nr.factor == 2. Chose c(1, 2, 3, 4)
#' @param method = "pairwise" by default. Others are possible (check ?contrast)
#' @param adjust = "tukey" by default. An adjustment of your p-values is usually required. 
#' @keywords 
#' @export
#' @examples
#' ZI.posthoc

ZI.posthoc <- function(fit, factors, nr.factors = 2, method = "pairwise", adjust = "tukey", selected.contrasts = F, contrasts){
  if (nr.factors == 1){
    df <- data.frame(summary(contrast(lsmeans(fit, ~ factors[1]),method=method,adjust=adjust)))
  } # if 1
  else if (nr.factors == 2){
    df <- data.frame(summary(contrast(lsmeans(fit, ~ factors[1]*factors[2]),method=method,adjust=adjust)))
  } # if 2
  else if (nr.factors == 3){
    df <- data.frame(summary(contrast(lsmeans(fit, ~ factors[1]*factors[2]*factors[3]),method=method,adjust=adjust)))
  } # if 3
  else if (nr.factors == 4){
    df <- data.frame(summary(contrast(lsmeans(fit, ~ factors[1]*factors[2]*factors[3]*factor[4]),method=method,adjust=adjust)))
  } # if 4
  if (selected.contrasts == T){
    df <- df[which(df$contrast %in% contrasts),]
  }
  df$symbol <- asterisks(df$p.value)
  return(df)
  
  } # function
