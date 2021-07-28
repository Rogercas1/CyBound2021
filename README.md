# CyBound2021

This is a repository for content related to Roger Castillo Ramos's CyBound 
research in the summer of 2021. 
The goal for this research is to build an R shiny app to allow farmer's to 
understand how their yield and nitrous oxide emissions are affected by 
fertilizer application. 

The three main directories are 

- docs/ 
- data/ 
- code/


## Online app

The online app to visualize the effects of nitrogen fertilizer rate amount
is available [here](http://167.71.158.10:3838/effects_of_fertilizer_rate/). 

## Data files

### `data/ym/basswood_2020.geojson`

Basswood 2020 (Maize) smoothed yield in [GeoJSON](https://geojson.org/) format.

* `yieldMgHaMean`: smoothed yield mean in Mg/Ha.
* `yieldMgHaVar`: smoothed yield variance in (Mg/Ha)^2.

## `data/ym/basswood_2020.csv`

Basswood 2020 (Maize) smoothed yield in ggplot-enabled long format.

* `yieldMgHaMean`: smoothed yield mean in Mg/Ha.
* `yieldMgHaVar`: smoothed yield variance in (Mg/Ha)^2.
