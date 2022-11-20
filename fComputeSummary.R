# Description:
# This function computes some of the summary statistics available for the solution

fComputeSummary <- function(s, total_area, PU_size, run_name, Cost) {
  
  summary <- s %>% 
    tibble::as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    dplyr::filter(solution_1 == 1) %>% 
    dplyr::summarize(sum_area = nrow(.), 
                     total_cost = sum(!!sym(Cost))) %>%  # total area and cost (should be the same)
    dplyr::mutate(percent_area = sum_area*100/total_area, 
                  num_pu = nrow(s %>% as_tibble() %>% filter(solution_1 == 1)), 
                  run = run_name)
  
  return(summary)
}