# COVID-19 Tracking Shiny App
## STAT 440 - Statistical Data Management [SPRING 2022]
A Shiny application that provides a visual interface for COVID-19 trends, including Confirmed, Death, Recovered cases, and estimated Effective Reproduction Number (Rt) trend.
> Ikgyu Shin (ikgyus2@illinois.edu)


## Table of Contents
* [General Information](#general-information)
* [Minimum Requirements](#minimum-requirements)
* [Possible Improvements](#possible-improvements)
* [Acknowledgements](#acknowledgements)


## General Information

The project utilizes data sourced from the CSSEGISandData's GitHub repository, which hosts the widely recognized COVID-19 data maintained by the Johns Hopkins University Center for Systems Science and Engineering (CSSE). It encompasses daily updated figures on confirmed cases, deaths, and recoveries. This analysis involves the R programming language and heavily utilizes the dplyr, tidyr, and other packages. Key functions are implemented to preprocess the data—transforming, mutating, and filtering relevant details. The datasets for the U.S. are narrowed down to exclude non-state territories, and datasets for South Korea are handled separately. Additionally, calculations for the virus's reproduction number (Rt), which is essential for understanding its transmissibility, are provided for both the US and South Korea.

## Minimum Requirements

The Shiny app or dashboard should only use data with a URL. There must be at least 1 data visualization and at least 1 summary table or tibble returned.

- What problem does it (intend to) solve? | What is the purpose of your project?
  - Data Preprocessing: To process raw data streams to create clear, workable datasets representing the daily incidence of confirmed cases, deaths, and recoveries in the U.S. and South Korea.

- Brief explanation of your approach
  - Epidemic Analysis: To determine the reproduction number (Rt) of the COVID-19 virus for different states in the US and South Korea. This metric provides insights into the virus's contagiousness, helping policymakers decide on containment measures.


## Possible Improvements

**Improvements:**
- Automated Data Updating
  
- Integration of More Countries


**How:**
-  Implementing a script to automatically fetch the latest data from the CSSEGISandData's GitHub repository, ensuring the analysis is always based on the most recent data without manual input.
  
- Expanding the code structure to accommodate data from other countries enhances the analysis's global applicability and provides a more comprehensive view of the pandemic.


## Acknowledgements

- *This project borrowed knowledges from __[Official R Shiny website](https://shiny.posit.co/)__.*
