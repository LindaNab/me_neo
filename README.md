# measerror-msm

## Version 0.1.0
Code of the simulation study from the manuscript 'Internal validation data sampling strategies for exposure measurement error correction:  a study of visceral adipose tissue measurements replaced by waist circumference measurements' by Linda Nab et al.

## Generating data and running simulation study
The R script 1_exe.R in ./rcode can be used to execute the sim study. Data will be saved in ./data/output.

## Processing results of simulation study (under construction)
The R code in ./src/tabular creates tables with the results of the simulation study. The table that is produced by the script can be used in LaTeX files. The script uses the .rds files available in ./data/processed. The output of the script is saved in .results/tables.

The R code in ./src/visualisation creates figures depicting the results of the simulation study. The script produces .png files. The scripts uses the .rds files available in ./data/processed. The output of the script is saved in .results/figures.

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
│   ├── summarised     <- summaries of the output
│   └── output         <- The original, generated data files that were analysed
├── docs               <- Documentation notebook for users (HW)
│   ├── manuscript     <- Manuscript source, e.g., LaTeX, Markdown, etc. (HW)
│   └── reports        <- Other project reports and notebooks (e.g. Jupyter, .Rmd) (HW)
├── results
│   ├── figures        <- Figures for the manuscript or reports
│   ├── tables         <- Tables for the manuscript or reports
│   └── output         <- Other output for the manuscript or reports
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
