# freq-paper   
Reproducibility repository for a manuscript on brain metastases frequencies and classification accuracy of medicare claims thereof

# Requirements   

- R Version 3.4.
  - tidyverse
  - pandoc
- GNU Make 3.82 (Copyright (C) 2010 Free Software Foundation, Inc)
- Git (optional)

# Notes  

- It will take a few hours to run the whole project on a single processor 

# Instructions 

Reproducing this work approximately follows two steps: 
- Put data in the correct folder
- Run 'make'

## Getting the project

Either download the project [here](link), or use `git clone https://github.com/mustafaascha/freq-paper` to clone this repository. 

## Data  

Data is not provided, though data files may be placed in the `seerm` folder to reproduce this analysis. 

File structure is loosely organized so that there are two R packages, `augur` to read the files and `frequencies` to make the manuscript. Noting that `seerm` is an empty directory where we will place the data, the structure looks like this: 

(top level)
├── augur
│   └── ...
├── bibliography.bib
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
└── years.png
  
After adding the data, it will appear as follows: 


(top level)
├── augur
│   └── ...
├── bibliography.bib
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
└── years.png

At that point, use the command `make`. Depending on how much data you have, it may take a computer with 40GB or more to run. 














