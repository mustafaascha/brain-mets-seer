# Reproducibility repository for a SEER-Medicare study of brain metastases

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1300053.svg)](https://doi.org/10.5281/zenodo.1300053)

This is a repository enabling reproduction of results from a manuscript on brain metastases. The manuscript examines frequencies (incidence proportions) and classification accuracy of Medicare claims for identifying brain metastasis status during cancer staging workup.

## Notes  

This project will take several hours to run on a single-processor machine with 64GB of memory. You can run `make` with the `-jN` option (where N is some integer) to go through claims files in parallel, but that's not recommended because it's best to have all claims evaluated before letting a thread start munging. Manuscript analysis was run on Red Hat Enterprise 7.4 (Maipo).     

## Contents

- `augur` - This R package reads SEER-Medicare data and extracts relevant rows from the claims data.      

- `documentation` - These tables are used to recode e.g. histology, diagnoses.     

- `extraction-scripts` - This folder contains R scripts to extract relevant claims data.     

- `frequencies` - This R package supports analysis and manuscript preparation.    

- `munge` - These scripts convert the data to an analyzable format.     

- `reports` - These scripts are the last step before results are presentable and can be easily used in RMarkdown.    

## Instructions 

Reproducing this work approximately follows two steps: 
- Put lung, breast, and skin cancers data in the correct folder (`seerm`)
- Run `make`. To see what will be run, use `make --just-print`.      

## Depends

In addition to R 3.4.2 (2017-09-28) and a variety of R packages, this work depends on GNU `make` 4.2.1 and `pandoc` 1.19.2.1.          

## Getting the project

Either download the project [here](https://github.com/mustafaascha/brain-mets-seer/archive/master.zip), or use `git clone https://github.com/mustafaascha/brain-mets-seer` to clone this repository. 

## Data  

Data is not provided, though data files may be placed in the `seerm` folder to reproduce this analysis. 

File structure is loosely organized so that there are two R packages, `augur` to read the files and `frequencies` to make the manuscript. Noting that `seerm` is an empty directory where we will place the data, the structure looks like this: 

(top level)   
├── augur    
│   └── ...    
├── documentation    
│   └── ...    
├── extraction-scripts    
│   └── ...    
├── frequencies    
│   └── ...    
├── LICENSE    
├── Makefile    
├── munge    
│   └── ...    
├── README.md    
├── reports    
│   └── ...    
├── seerm     
└── tables-and-figures.Rmd      
  
After adding the data, it will appear as follows: 


(top level)    
├── augur     
│   └── ...     
├── documentation    
│   └── ...    
├── extraction-scripts    
│   └── ...    
├── frequencies    
│   └── ...    
├── LICENSE    
├── Makefile    
├── munge    
│   └── ...    
├── README.md    
├── reports    
│   └── ...    
├── seerm     
│   ├── CCflag07.txt.gz    
│   ├── ...    
│   ├── dme07.file01.txt.gz    
│   ├── ...    
│   ├── medpar07.txt.gz    
│   ├── ...    
│   ├── nch07.file001.txt.gz    
│   ├── ...    
│   ├── outsaf07.file001.txt.gz    
│   ├── ...    
│   ├── pdesaf07.file01.txt.gz    
│   ├── ...    
│   ├── pedsf.breast.cancer.file01.txt.gz    
│   ├── pedsf.breast.cancer.file02.txt.gz    
│   ├── pedsf.lung.cancer.file01.txt.gz    
│   ├── pedsf.lung.cancer.file02.txt.gz    
│   ├── pedsf.breast.cancer.file01.txt.gz    
│   └── CCflag07.txt.gz    
└── tables-and-figures.Rmd       

At that point, use the command `make`. Depending on how much data you have, it may take a computer with 40GB or more to run. 














