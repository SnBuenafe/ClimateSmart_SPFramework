# Climate-smart spatial planning

The layers required for spatial planning were prepared using repository from `MathMarEcol/SpatialPlanning`. This repository makes use of the Western Pacific as its template, but most analyses can be replicated for different planning regions, whether marine or terrestrial.

The layers were created in `02_SpatPlan_Master_WestPac.R` and loaded in `03_SpatPlan_Master_Preliminaries.R`.

For a spatial planning project, the following are required:
1. Planning Region - Western Pacific
2. Conservation Features - We used AquaMaps
3. Cost Layer - We used Watson's Dataset
4. Climate Layers - We created multi-model ensembles across 3 climate scenarios for 3 oceanic variables. From there, we calculated layers of 4 climate metrics (created in `01_SpatPlan_ClimateMetrics.R`)
5. `prioritizr` to create the spatial planning problems
6. A solver (e.g. Gurobi) to solve the spatial planning problems


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

__Iterative runs__ (`08_SpatPlan_WestPac_SuppRuns_Iterations.R`)

Ran all possible options across all the the 4 themes.
