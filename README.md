# measerror-msm

Version 0.1.0
Code of the simulation study from the manuscript 'Internal validation data sampling strategies for exposure measurement error correction:  a study of visceral adipose tissue measurements replaced by waist circumference measurements' by Linda Nab et al.

## Generating data and running simulation study
The R code in /src/dgm can be used to generate the data and the R code in /src/sim can be used to run the simulation study. Data will be saved in /data/raw.

## Processing results of simulation study
The R code in /src/table that comprises the results of the simulation study. The table that is produced by the script can be used in LaTeX files. 

The script will use the .rds files available in ./data/processed.

Download the repository by using:
```console
git clone https://github.com/LindaNab/me_neo_simstudy
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
│   ├── summarised     <- summaries of raw data
│   └── raw            <- The original, generated data files
├── docs               <- Documentation notebook for users (HW)
│   ├── manuscript     <- Manuscript source, e.g., LaTeX, Markdown, etc. (HW)
│   └── reports        <- Other project reports and notebooks (e.g. Jupyter, .Rmd) (HW)
├── results
│   ├── figures        <- Figures for the manuscript or reports
│   ├── tables         <- Tables for the manuscript or reports
│   └── output         <- Other output for the manuscript or reports
└── src                <- Source code for this project
    ├── dgm            <- Scripts that define the dgm
    ├── sim            <- Scripts used to run the simulation study
    ├── sumsim         <- scripts and programs to summarise data
    ├── external       <- Any external source code, e.g., pull other git projects, or external libraries
    ├── tools          <- Any helper scripts go here
    ├── tabular        <- Scripts for tabularizing your results
    └── visualisation  <- Scripts for visualisation of your results, e.g., matplotlib, ggplot2 related.


```


## License

This project is licensed under the terms of the [MIT License](/LICENSE.md)

## Citation

Please [cite this project as described here](/CITATION.md).
