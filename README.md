
# Reproducing "A Longitudinal Study of Perceived Social Position and Health-related Quality of Life"

An R reproduction of the panel-data analysis in *"A Longitudinal Study of Perceived Social Position and Health-related Quality of Life"* (Gugushvili & Jarosz, 2024) 
Using three waves of the POLPAN dataset. 
The original analysis was written in Stata; this repository translates it to R and documents the steps needed to prepare the data.

## Overview

The aim of this project is to translate the code of the following paper  [Gugushvili, Alexi and Ewa Jarosz. 2024] 
"A Longitudinal Study of Perceived Social Position and Health-related Quality of Life." Social Science & Medicine 340, 116446. 
(https://doi.org/10.1016/j.socscimed.2023.116446). 
The code is written in R and applies panel data regression to the PolPan dataset. 
The goal of the paper is to see whether there is an association between health-related quality of life and subjective self-perceived social position. 
The data used comes from the PolPan dataset and uses three waves (2008, 2013, 2018). The references to the dataset are the following: Zelinska, O., Gugushvili, A., Bulczak, G., Tomescu-Dubrow, I., Sawiński, Z., & Słomczyński, K. M. (2021). 

The Polish Panel Survey (POLPAN) dataset:
Capturing the impact of socio-economic change on population health and well-being in Poland, 1988–2018. Data in brief, 35, 106936. 
We find out that the Stata code wasn't perfectly reproducible, we had to create a script that transforms the crude data into something that could be treated by the R functions. 
For example, in the original dataset, negative values were present, and the Stata code without any modification wasn't taking those entries out. Variable names from the dataset were also quite different from what was presented in the code. 
Stata also has a few wrappers which reduce the flexibility of the code but at the same time make things easier by automatically sorting NaN values.

## Repository structure

- `DATA_CLEANER.ipynb` — Jupyter/Python notebook (pandas) that selects only the variables used in the study from the full POLPAN file, so the analysis does not load thousands of unused columns. It reads the raw POLPAN CSV and writes a smaller, cleaned CSV.
- `stata_to_r.R` — the R translation of the original Stata analysis. 
                    It recodes the Nottingham Health Profile (NHP) items and its six subscales (energy level, pain, emotional reaction, sleep, social isolation, physical abilities), 
                    builds the model variables, reshapes the data to long panel format, and estimates the panel regressions (OLS, fixed effects, random effects, and hybrid `wbm` models) along with descriptive statistics and density plots.
- `main.py` — default project entry-point stub; not used by the analysis.
- `pyproject.toml`, `uv.lock`, `.python-version` — Python environment, managed with [uv](https://docs.astral.sh/uv/) (Python ≥ 3.13; pandas, jupyter).
- `renv.lock`, `renv/`, `.Rprofile` — R environment, managed with [renv](https://rstudio.github.io/renv/) (R 4.5.3).

## Requirements and setup

**Python (uv).** From the project directory:

```bash
uv sync
```

**R (renv).** From an R session in the project directory:

```r
renv::restore()
```

The R script uses the following packages: `dplyr`, `tidyr`, `plm`, `panelr`, `marginaleffects`, `ggplot2`, `patchwork`, `stargazer`, `modelsummary`.

## How to run

1. **Get the data.** The POLPAN data is **not** included in this repository and must be obtained separately from the POLPAN project (see the dataset reference in the Overview above).
2. **Clean the data.** Open `DATA_CLEANER.ipynb`, update the input path (the full POLPAN CSV, e.g. `Polpan1988_2018.csv`) and the output path (e.g. `cleaned_data3.csv`) to match your machine, then run the notebook to produce the cleaned file.
3. **Run the analysis.** Open `stata_to_r.R`, update the `read.csv(...)` path to point to your cleaned CSV, then run the script.

## Results and limitations

This is a **partial reproduction**. The estimates produced by the R code do **not** exactly match the results reported in the published paper, and accounting for the remaining differences is still an open issue.

A few practical points to be aware of:

- **Data is not redistributed here.** You must obtain POLPAN separately before anything will run.
- **Absolute paths.** Both `DATA_CLEANER.ipynb` and `stata_to_r.R` currently use file paths specific to the original author's machine, so they must be edited before running on another computer.
- **R lockfile.** `renv.lock` currently pins only `renv` itself, not the analysis packages listed above. 
                   To fully freeze the R environment, install those packages and run `renv::snapshot()`.

## AI Usage

AI assistance was used for supporting to understand certain R functions, to help understand some of the formulas used and to fix typos. 
It was designed the analysis and generate the results by our team members.

## Team Member

- Luka de Caters (487317)
- Stanislaw Godlewski (473016)
- Hayeong Jae (488101)
