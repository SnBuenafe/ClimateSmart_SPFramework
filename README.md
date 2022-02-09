# SpatialPlanning: Western Pacific

This workflow was adapted from MathMarEcol/SpatialPlanning repo, with modifications specific to the region (Western Pacific).

The entire workflow for this spatial planning project can be followed in `SpatPlan_Master_WestPac.R`.

For a spatial planning project, the following are required:
1. Planning Region - Western Pacific
2. Conservation Features - We used AquaMaps
3. Cost Layer - We used Watson's Dataset
4. Climate Layers - We created multi-model ensembles across 3 climate scenarios for 3 oceanic variables. From there, we calculated layers of 4 climate metrics
5. `prioritizr` to create the spatial planning problems
6. A solver (e.g. Gurobi) to solve the spatial planning problems