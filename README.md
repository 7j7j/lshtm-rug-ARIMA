# lshtm-rug-ARIMA
Materials on forecast package and ARIMA, supporting LSHTM RUG talk 17 Dec 2020


Please send questions & comments to julia.shen1 (at) lshtm (dot) ac (dot) uk

# Required packages

forecast: we will use the forecast::arima wrapper and related functions in this package, which enhance the base::arima function included in the default R stats library
tidyverse: recommended if not strictly necessary for practical exercises, though my tidying + dataviz processes use dplyr, ggplot2, etc
here: used for easier filepaths

# Directory

1. Case DATA in "ARIMA-intro.csv"
 ... with more contextual metadata and calculations in "ARIMA-intro-meta.xlsx"

2. Case CODE in "ARIMA-intro.R" available 

3. Contextual and theoretical SLIDES, including important fundamentals of underlying statistical theory, in "ARIMA-slides.pdf" including a list of references and further useful reading

4. Original inspiration for this practice case comes from Mark Green et al's article "Could the rise in mortality rates since 2015 be explained by changes in the number of delayed discharges of NHS patients?"
in the BMJ Journal of Epidemiology & Community Health at https://jech.bmj.com/content/71/11/1068
... with many thanks to Mark Green, and co-authors Danny Dorling, Jonathan Minton, and Kate E. Pickett for the applied example in English health geography that surfaces such important and interesting policy questions about service performance and equity. This is true even if our modelling methods for such complex links are more problematic than we would like.

This README.md + MIT license for open-access, attributed usage of these materials - please minimise commercial gatekeeping

# Original data sources for England

- ONS, Mid-year population estimates: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
 
- ONS, Deaths registered monthly in England and Wales: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland

- NHS England and NHS Improvement, Delayed Transfers of Care (DTOCs) from August 2010 - present: https://www.england.nhs.uk/statistics/statistical-work-areas/delayed-transfers-of-care/

# Acknowledgments
Thanks to the Health Foundation, UK Economic & Social Research Council, and LSHTM Public Health & Policy faculty, for funding and hosting me.
