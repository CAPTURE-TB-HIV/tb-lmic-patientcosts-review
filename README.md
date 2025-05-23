# tb-lmic-patientcosts-review

Analytic code for meta-analyses associated with a systematic review of out-of-pocket expenses for persons with tuberculosis (TB) in low- and middle-income countries (LMICs).

## Project description

This project has two parts:  
    1. a systematic review of TB-related costs from the patient perspective within LMICs from 2015 to 2025.  
    2. meta-analysis of direct medical, direct non-medical, and indirect costs.
    
## Systematic review 

The detailed protocol for the systematic review is registered with [PROSPERO](https://www.crd.york.ac.uk/PROSPERO/view/CRD42025159307). 

We identified and extracted data from 119 unique prevalence reviews using [Covidence systematic review software](https://covidence.org). The initial raw data set extracted from these papers is archived in the `fullDataRaw.rds` datafile. 

## Data cleaning 

Before analyzing the extracted data we performed various data cleaning steps. These included:  
    1. revising column names and standardizing the variable values for efficient coding.  
    2. confirming no data that violated the exclusion criteria of the systematic review.  
    3. ensuring no logical inconsistencies across the extracted data. 
    
Data cleaning can be replicated by running the `cleanData.R` file using the `fullDataRaw.rds` datafile. This script will generate a clean dataset, which is stored in the `fullDataClean.rds` datafile.

## Descriptive analysis 
We first aimed to understand the distributions of the data we collected. We designed two scripts, `descriptiveAnalysisFigures.R` and `descriptiveAnalysisTables.R` to explore the data, in sum and within stratifications. The outputs of these scripts are saved to `outputs/descriptive` folder.
