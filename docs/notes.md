# Meeting Notes

This document will contain notes of various meetings throughout the summer. 

## 2021-07-29

Continue practicing presentation

## 2021-07-28

- Possibly presenting to Science Bound
- TODO: Roger will create emissions figures
  - Red color
  - Use emissions() function
- TODO: Nick will determine N2O synthetic data
- TODO: Jarad will create nitrate leaching synthetic data
- TODO: Roger will put basswood data in code/ directory


## 2021-07-26

Jarad todo:
- Get digital ocean droplet up and running with app
- Ask someone about NO2 emission vs yield

Roger todo:
- Shiny app development
  - Yield function
  - add yield heatmap
  - modify yield heatmap for wheat in Mexico
  - modify "after" yield heatmap based on slidy input



## 2021-07-19

Roger todo:
 - copy code/app.R 
 - modify to create N fertilizer rate sliderInput
 - modify output to have 2 sentences: one for yield and one for emission
 
Nick todo:
 - find yield equation via data or figure

Jarad todo:
 - ask Matt Helmers about nitrate leaching as a fxn of N fertilizer rate
 - follow-up with Luis about yield map

## 2021-07-15

### Jarad's notes

GHG

Nitrate leaching- breakpoint at optimal N
Carbon increases with nitrogen at optimal N

Carbon accrual plateaus
[DayCENT](https://www2.nrel.colostate.edu/projects/daycent/)/[CENTURY](https://www.cgd.ucar.edu/vemap/abstracts/CENTURY.html)/[Agro-IBIS](https://lter.limnology.wisc.edu/project/agro-ibis) 
- biogeochemical model

Agronomic optimum N
- changes year to year (weather)
- Changed by landscape position 

Cover crops

Nitrogen
- ammonium
- Nitrate (mobile in soils)

Fertilizer
- organic: legumes, manure
- Inorganic: urea ammonium nitrate, anhydrous ammonia, 

Ask [Chad Hart](http://www2.econ.iastate.edu/faculty/hart/) about carbon markets

Ask [Matt Helmers](https://www.abe.iastate.edu/matthew-helmers/) about nitrate leaching 

Phosphorus doesn’t leach as quickly

Nitrogen rate trials
- [John Sawyer](https://www.agron.iastate.edu/people/john-sawyer)
- [Mike Castellano](https://www.agron.iastate.edu/people/michael-castellano)
- [Sotirios Archontoulis](https://www.agron.iastate.edu/people/sotirios-archontoulis)




### Nick's notes


Meeting with Marshall

-For the Shiny app
  -add nitrogen leeching  , goes up after "agronomic optimal nitrogen"
  -add soil carbon level  , goes down after "agronomic optimal nitrogen"

-emphasis on predicting agronomic optimal nitrogen, spatially, temporally, it's weather-dependent

-future project: a spatial-temporal plot showing agronomic optimal nitrogen over a field changing over time depending on some covariates...

-daycent a biogeochemical model, andy van lukey marshall's colleague, daycent provides model-based greenhouse gas emissions
	- can provide soil carbon "data"

-marshall will send us some other papers, matt helmers, james, nitrogen leeching, phil robertson breakpoint curve paper?

-talk to chad hart about carbon markets


## 2021-07-14

Previously

- Roger
  - Abstract briefly describes what was done and what results were found
  - No blockchain module in PEWI canvas course, but in [Neh Batwara's CC](https://lib.dr.iastate.edu/creativecomponents/708/)
  - Watched [Ecosystem Market Series: Data Platforms](https://www.youtube.com/watch?v=AN3PI3YBRsI&t=1s&ab_channel=ILSustainableag)
    - Not too informative/useful
  - Through Ch 12 of R4DS
- Jarad
  - updated Finding a line document with exponential relationship
  - function to calculate emissions from fertilizer application rate
- Nick
  - added shiny app and documentation

For next time

- Roger 
  - complete Summer Research Symposium application by tomorrow evening
  - R4DS chapters 17-21
  - look at [geom_tile](https://ggplot2.tidyverse.org/reference/geom_tile.html) to create yield maps


## 2021-07-12

Previously

- derived formulas for linear regression
- data wrangling/analysis of flights data
  - how to remove NAs
    - `na.omit()`
    - `filter()` with `!is.na()`
  - test using `lm()`
- Talked about [Illinois Sustainable Ag Partnership Farming for the Future videos](https://ilsustainableag.org/ecomarkets/) 

For Wednesday

- Jarad will call Marshall McDaniel about meeting Thursday at 10am or 1pm
- Nick will try to create a template shiny app
- Jarad will look into getting data from [AEE paper](https://lter.kbs.msu.edu/docs/robertson/millar-et-al-2018-aee.pdf)
- Roger will create functions to find intercept and slope
- Roger will progress through Chapter 13 in R4DS
- All will watch [Ecosystem Market Series: Data Platforms](https://www.youtube.com/watch?v=AN3PI3YBRsI&t=1s&ab_channel=ILSustainableag)
- Jarad will update "Finding a line" document with exponential relationship
- Jarad will send email to Nancy Grudens-Schuck about Canvas module on PEWI Blockchain  






## 2021-07-02

Conversation with Mark Mba-Wright

- Biofuel
- Improving sustainability of food chain using blockchain
  - e.g. milk history
- Blockchain to track farmer history (perhaps anonymously)


Benefits of blockchain

- Traceability
- Accountability
- Anonymity

Renewable fuel standard

- Renewable identification number
  - willing to pay for ethanol
  
Mark will send Iowa Energy Center proposal

Guan Yong from Electrical and Computer Engineering is blockchain expert

Possible project:

Generate fictitious farmers and companies and develop a blockchain technology
to understand how the market would function.

Roger will talk to Lisa Schulte-Moore and Nancy Grudens-Schuck about PEWI blockchain. Contact Richard and student about showing PEWI. 

How should government be involved in carbon markets/credits? 
module. 




## 2021-07-01

From last meeting:

- finished swirl programming lessons
- websites for practicing code???
- line through points
  - slope is calculated via SXY/SXX

For next meeting:

- Aim to get through chapter 8 of [R4DS](https://r4ds.had.co.nz/)
- Watch Jill Euken video





## 2021-06-30

Since Monday

- swirl R programming lessons 1-9
- watched Part 1
- regression line

For Thursday

- finish swirl R programming lessons
- skim other videos
- think about regression line



## 2021-06-13

Our first meeting was primarily a brainstorm to determine a direction for the
summer project. 
This discussion is summarized in brainstorm.md
