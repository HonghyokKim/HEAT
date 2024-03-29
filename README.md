# An R package, HEAT (Heatwave effect Estimation via Adjustment for Temperature)

Update: Aug 25, 2023

We have corrected an error that arose when the function "createADJforHT" was used to create non-piecewise temperature variables (piecewise=FALSE).

Update: Aug 30, 2023

Validation was conducted for other definitions of heatwaves.


This R package allows researchers to use the novel method to estimate the effect of heatwaves on a health outcome, proposed by Dr. Honghyok Kim (University of Illinois at Chicago) in the following paper. Each function embeded in this package has a description file. Use "?functionname" in an R environment for detail.

The pre-print is available on arXiv.org. The paper has been submitted to a scientific journal for peer-reviewed publication:


**"On adjustment for temperature in heatwave epidemiology: a new method and toward clarification of methods to estimate health effects of heatwaves"** by
Honghyok Kim 1, Michelle L. Bell 2


1.Division of Environmental and Occupational Health Sciences, School of Public Health, University of Illinois Chicago, Chicago, Illinois, USA

2.School of the Environment, Yale University, New Haven, CT, USA


**Validation**

-This package has been validated for replicating the results presented in the paper. An R code and data set for replication is provided here: daily time-series dataset for Seoul, South Korea (2006-2013). Potential investigators can learn this package with them and are welcome to follow the methods presented in the paper.

-This package includes heatwave definitions that were not analyzed in the original paper. In case potential investigators want to use other heatwave definitions, we highly encourage you to understand how this method works first. Please make sure if variables generated are appropriate for their purposes. If you have any questions, please email me at honghyok@uic.edu. 
