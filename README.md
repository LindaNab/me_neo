# The me_neo repository

## Version 0.1.0
This repository contains the code and simulation output of the simulation study accompanying the manuscript 'Internal validation data sampling strategies for exposure measurement error correction:  a study of visceral adipose tissue measurements replaced by waist circumference measurements' by Linda Nab et al.

## Running simulation study
This simulation study is ran with use of [GitHub Actions](https://github.com/features/actions). A particular (series of) job(s) can be executed by using main.yml available in the directory .github/workflows. The YAML is used to run the R script execute_simstudy_gha.R, available in the directory ./rcode/exe, and uses the arguments `scen_num` (number of scenario, see next section) and `rep` (number of replications). After the job has run, a new branch is created named patch_Sscen_num. This newly created branch contains the simulation output for that particular simulation scenario `scen_num`.

The simulation output on which the results of the paper are based is available in the directory [./data/output](./data/output). 

## Simulation scenarios
The simulation study is based on 41 different data generating mechanisms, referred to as S0-S40 (`scen_num`). These different data generating mechanisms are defined by the function `datagen_scenarios()` available in rcode/dgm/sim_scen.R. The data generating mechanisms vary by: the explained variance of the measurement error model (`R_squared`/`tau`); the skewness of the residual errors of the model for visceral adipose tissue given sex, age and total body fat (`skewness`/`lambda`); whether the measurement model is linear or non-linear (`linear`). For each of these 41 data generating mechanisms, 5000 data sets were generated.

Each one of these data sets were analysed using 60 different analyses. These different analysis scenarios are defined by the function `analysis_scenarios()` available in rcode/dgm/sim_scen.R. The analysis scenarios vary by: how the data in the validation sample is sampled (`sampling_strat`); which (measurement error correction) is applied (`method`); the percentage of individuals in the validation sample (`size_valdata`).

The name of the output files (in ./data/output) of the simulation study are contructed as follows:
size_valdata_(`size_valdata`*100)/method_`method`/`scen_num`_`sampling_strat`.Rds

## Processing results of simulation study (under construction)
The R script ./sumsim/process_sim_output.R processes the simulation output in ./data/output (e.g., adds column names and takes the square root of model based variances). The processed simulation output is saved in ./data/processed holding the same directory structure as the unprocessed simulation output. 

The R script ./rcode/sumsim/sum_processed_output.R summarises the processed output of the validation study and saves the summary as ./results/summaries/summary.Rds. 

The R code in ./rcode/tabular creates tables with the results of the simulation study. The table that is produced by the script can be used in LaTeX files. The script uses the .rds files available in ./data/processed. The output of the script is saved in .results/tables.

The R code in ./rcode/visualisation creates figures depicting the results of the simulation study. The script produces .png files. The scripts uses the .rds files available in ./data/processed. The output of the script is saved in .results/figures.

Download the repository by using:
```console
git clone https://github.com/LindaNab/me_neo
```

This script is tested on:


With attached packages:


## Project organization

```
.
├── .gitignore
├── CITATION.md
├── LICENSE.md
├── README.md
├── config             <- Configuration files
├── data
│   ├── output         <- The original results of each run
│   └── processed      <- The processed output of each run
├── docs               <- Documentation notebook for users (HW)
│   ├── manuscript     <- Manuscript source, e.g., LaTeX, Markdown, etc. (HW)
│   └── reports        <- Other project reports and notebooks (e.g. Jupyter, .Rmd) (HW)
├── results
│   ├── figures        <- Figures for the manuscript or reports
│   ├── output         <- Other output for the manuscript or reports
│   ├── summaries      <- (raw) summary of the simulation output
│   └── tables         <- Tables for the manuscript or reports
└── rcode              <- rcode for this project, 1_exe.R in root executes the simulation study
    ├── analysis       <- Scripts that run the analyses
    ├── dgm            <- Scripts that define the dgm
    ├── sim            <- Scripts used to run the simulation study
    ├── sumsim         <- Scripts and programs to summarise data
    ├── external       <- Any external source code, e.g., pull other git projects, or external libraries
    ├── tools          <- Any helper scripts go here
    ├── tabular        <- Scripts for tabularizing your results
    └── visualisation  <- Scripts for visualisation of your results, e.g., matplotlib, ggplot2 related.


```


## License

This project is licensed under the terms of the [MIT License](/LICENSE.md)

## Citation

Please [cite this project as described here](/CITATION.md).
