# Description
# This function creates Cohen's Kappa Correlation Matrix

fGetCorrMatrix <- function(list_plans) {
  
  y = 1
  s_matrix <- list() # empty list
  for(i in 1:length(list_plans)){
    for(j in 1:length(list_plans)){ # do pairwise comparisons for all solutions
      kappa_temp <- irr::kappa2(bind_cols(list_plans[[i]], list_plans[[j]]))
      kappa_corrvalue <- kappa_temp$value # store coefficient
      kappa_pvalue <- kappa_temp$p.value # store pvalues
      s_matrix[[y]] <- cbind(colnames(list_plans[[i]]), # first plan
                             colnames(list_plans[[j]]), # second plan
                             kappa_corrvalue, # correlation value
                             kappa_pvalue) # p value
      y = y+1
    }
  }
  
  s_matrix_all <- do.call(rbind, s_matrix) %>% 
    tibble::as_tibble()
  colnames(s_matrix_all)[1:2] <- c('plan1','plan2')
  
  matrix <- s_matrix_all %>% 
    tibble::as_tibble() %>% 
    dplyr::select(-kappa_pvalue) %>% 
    tidyr::pivot_wider(names_from = plan2, values_from = kappa_corrvalue) %>% 
    as.matrix()
  
  return(matrix)
}