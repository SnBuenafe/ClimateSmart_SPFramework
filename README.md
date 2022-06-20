# Climate-smart spatial planning

The layers required for spatial planning were prepared using repository from `MathMarEcol/SpatialPlanning`. This repository makes use of the Western Pacific as its template, but the main framework can be replicated for different planning regions, whether marine or terrestrial.

The layers were created in `02_SpatPlan_Master_WestPac.R` and loaded in the scripts from `04` onward using code from `03_SpatPlan_Master_Preliminaries.R`.

We advise against recreating the output layers using code from `01-02` since the calculations are computationally expensive. Instead, it is recommended to start reproducing the script from `04` onward. The scripts are independent, allowing the user to run them in their preferred order.

For a spatial planning project, the following are required:
1. Planning Region - Western Pacific
2. Conservation Features - We used AquaMaps [AquaMaps](https://www.aquamaps.org/)
3. Climate Layers - We created multi-model ensembles across 3 climate scenarios for 3 oceanic variables. From there, we calculated layers of 4 climate metrics (created in `01_SpatPlan_ClimateMetrics.R`)
4. `prioritizr` to create the spatial planning problems [prioritizr](https://prioritizr.net/)
5. A solver (e.g. Gurobi) to solve the spatial planning problems [Gurobi](https://www.gurobi.com/)

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


__"Approach" theme__ (`07_SpatPlan_WestPac_Runs_ApproachTheme.R`)

Exploring different ways to incorporate climate layers into spatial planning:

1. "Feature" approach: treats climate layers as features (the same way as biodiversity features)
2. "Percentile" approach: retains only "climate-smart" areas of each of the biodiversity features
3. "Climate priority area" approach: locks in the most "climate-smart" areas of each of the biodiversity features and still protects the rest of the features' distributions
4. "Penalty" approach: treats climate layers as linear penalties (e.g. penalizing selection of areas of high climate warming)

__"Metric + Approach" theme__ (`07.5_SpatPlan_WestPac_Runs_MetricApproachThemesSupplementary.R`)

Rerunning the Metric and Approach themes for all metrics and all approaches.

__Iterative runs__ (`08_SpatPlan_WestPac_SuppRuns_Iterations.R`)

Run all possible options across all the the 4 themes.

The solutions ran form `04-07.5` could also be ran using this script, but in `04-07.5` we singled out solutions that were reported in the manuscript to highlight the importance of the indiviudal aspects and allow for reproducibility and greater comparability within the aspects.

Code for iterative runs can be set by the user, just make sure that it is consistent with all the other scripts. For our purposes, we used the following code found in `Output/nmds/df_groups.csv`.

__nMDS__ (`09_SpatPlan_WestPac_nMDS.R`)

Create nMDS plots for outputs in `08_SpatPlanWestPac_SuppRuns_Iterations.R`


## Shiny Application

We developed a Shiny App to allow the user to explore the framework personally based on their interests. In this application, we briefly describe the proposed framework, providing a summary of the manuscript. Further, the App enables the user to test out how climate-smart spatial plans for the Western Pacific change with the different choices in the climate-smart aspects explored here and compare them. This showcases that planning for climate change could be multidimensional and complex.

To run the shiny app,

1. Make sure that you have all the solutions for all runs by running `08_SpatPlan_WestPac_SuppRuns_Iterations.R` (or use the ones we generated in `Output/solutions`)
2. Go to the `shinyApp` folder.
3. Open `run_app.R`.
4. Run the script.


# Questions? Feedback?

Submit an issue or pull request, or email your questions to: tinbuenafe@gmail.com
