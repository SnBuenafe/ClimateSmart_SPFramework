# Climate-smart conservation planning framework

This repository details the analysis done on the framework proposed by Buenafe et al. (2023) (). The case study uses the Western Pacific as its planning domain, but the framework can be used on different planning regions, whether marine or terrestrial.

Climate layers were created using scripts in `Data/Climatology/` and `01_SpatPlan_ClimateMetrics.R`. All other data layers were prepared in `02_SpatPlan_Master_WestPac.R`. Layers are loaded into scripts with the prefix `04` onwards using code from `03_SpatPlan_Master_Preliminaries.R`.

We advise against recreating the output layers for the Western Pacific using code from `01-02` since the calculations are computationally intensive. The [Zenodo repository](https://doi.org/10.5281/zenodo.7553307) of this project should contain the `.rds` files of the data layers for the Western Pacific. Instead, it is recommended to start reproducing the script from `04` onward. The scripts are independent, allowing the user to run them in their preferred order.

For a spatial planning project, the following are required:
1. Planning Region - Western Pacific
2. Conservation Features - We used AquaMaps ([AquaMaps](https://www.aquamaps.org/))
3. Climate Layers - We created a multi-model ensemble across 3 climate scenarios for 3 marine variables. From there, we calculated layers of 5 climate metrics (see `01_SpatPlan_ClimateMetrics.R`)
4. `prioritizr` to create the spatial planning problems ([prioritizr](https://prioritizr.net/))
5. A solver (e.g. CBC - COIN-OR branch and cut) to solve the spatial planning problems ([CBC](https://dirkschumacher.github.io/rcbc/articles/rcbc.html))

This code can be adapted for any planning domain and any metric/s that the user needs.

## Climate-smart aspects explored

__"Scenario" theme__ (`04_SpatPlan_WestPac_Runs_ScenarioTheme.R`)

Using a single scenarios _versus_ multiple climate scenarios


__"Ensemble" theme__ (`05_SpatPlan_WestPac_Runs_EnsembleTheme.R`)

Creating climate layers using the ensemble's mean ("ensemble" approach) _versus_ creating individual climate layers for each of the models in the ensemble ("multi-model" approach)


__"Metric" theme__ (`06_SpatPlan_WestPac_Runs_MetricTheme.R`)

Using different climate metrics to create climate-smart spatial plans:

1. Rate of climate warming
2. Rate of ocean acidification
3. Rate of declining oxygen concentration
4. Climate velocity
5. Annual marine heatwave intensity
6. Combined climate-smart metric


__"Approach" theme__ (`07_SpatPlan_WestPac_Runs_ApproachTheme.R`)

Exploring different ways to incorporate climate layers into conservation planning:

1. "Feature" approach: protects climate-smart areas across the domain, regardless of whether they have any biodiversity value or not
2. "Percentile" approach: retains only climate-smart areas of each of the biodiversity features
3. "Climate priority area" approach: affords greater protection to high-value climate-smart areas of each of the biodiversity features and still protects the rest of the features' distributions
4. "Penalty" approach: treats climate layers as linear penalties (e.g. penalizing selection of areas of high climate warming)

## Other runs

__Supplementary runs__ (`08a-c_SpatPlan_WestPac_Runs_Supplement.R`)

Explores different approaches of incorporating climate metrics into spatial prioritization.

__Summary runs__ (`09a-b_SpatPlan_WestPac_Runs_Summary.R`)

Summarizing all the runs designed using different metrics across the four approaches.

__Sensitivity analysis__ (`10_a-d_SpatPlan_WestPac_Runs_Sensitivity.R`)

Sensitivity analysis done across the four approaches to identifying climate refugia.

__Iterative runs__ (`11_SpatPlan_WestPac_Runs_Iterations.R`)

Code for iterative runs can be set by the user, just make sure that it is consistent with all the other scripts. For our purposes, we used the following code found in `Output/nmds/df_groups.csv`.

__nMDS__ (`12_SpatPlan_WestPac_nMDS.R`)

Create nMDS ordination plots for all 432 solutions. 

## Shiny Application

We developed a Shiny App to allow the user to explore the framework personally based on their interests. In this application, we briefly describe the proposed framework, providing a summary of the manuscript. Further, the App enables the user to test out how climate-smart spatial plans for the Western Pacific change with the different choices in the climate-smart aspects explored here and compare them. This showcases that planning for climate change could be multidimensional and complex.

To run the shiny app,

1. Make sure that you have all the solutions for all runs by running `11_SpatPlan_WestPac_Runs_Iterations.R` (or use the ones we generated in `Output/solutions`)
2. Go to the `shinyApp` folder.
3. Open `run_app.R`.
4. Run the script.

## Session Info

R version 4.2.2 was used. We used macOS and Linux Mint to write the framework.

Packages:

1. prioritizr_7.1.1
2. terra_1.6-7
3. sf_1.0-9 
4. stars_0.5-6
5. tidyverse_1.3.2
6. VoCC_1.0.0
7. irr_0.84.1
8. rnaturalearth_0.1.0
9. purrr_0.3.4
10. proj4_1.0-11
11. RColorBrewer_1.1-3 
12. corrplot_0.92
13. irr_0.84.1
14. doParallel_1.0.17
15. ggridges_0.5.4 
16. magrittr_2.0.3
17. patchwork_1.1.1
18. BiocManager_1.30.18
19. raster_3.5-29 
20. ggplot2_3.4.1


# Questions? Feedback?

Submit an issue or pull request, or email your questions to: tinbuenafe@gmail.com
