# Description:
# This function computes some of the summary statistics available for the solution

fComputeSummary <- function(s, total_area, PU_size, run_name) {
  
  summary <- s %>% 
    tibble::as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    dplyr::filter(solution_1 == 1) %>% 
    dplyr::summarize(num_pu = nrow(.)) %>%  # total area
    dplyr::mutate(percent_area = num_pu*100/total_area, 
                  run = run_name)
  
  return(summary)
}
