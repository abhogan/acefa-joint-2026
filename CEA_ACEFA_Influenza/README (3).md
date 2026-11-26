# ACEFA — influenza vaccination economic evaluation (public R workflow)

**Status: first publication-ready *draft*; requires a local validation run and author review before public release.**

This repository contains R code for the Australian influenza LAIV/IIV economic evaluation using UNSW and University of Melbourne transmission-model outputs. The economic model, cost and QALY inputs, probabilistic sensitivity analyses, and manuscript figure-generating code are public; the source transmission-model simulation outputs are **not** distributed.

## Data availability and confidentiality

- **Restricted:** Original UNSW and UoM transmission-model `.dta` datasets. Contact the study team or data owners about access; availability is not guaranteed.
- **Not automatically public:** `df_cea`, `df_mean_epi`, `psa_results_full`, other simulation-level PSA/cache files and any derived data that may reveal the restricted model outputs. The `data_private/` and `outputs/` folders are ignored by Git.
- **Public code:** Cost/QALY calculations; inputs and distributional assumptions; analyses, tables and plots.
- **Illustrative inputs:** `data_example/` contains entirely synthetic values. These are **not** the study's results or estimates.
- **Other inputs:** The vaccine allocation CSV and Australian life table Excel workbook are not supplied with the original R-code attachment. The researcher must provide them. Verify life-table redistribution rights before publishing the workbook.

The repository is methodologically reproducible using synthetic inputs. Exact reproduction of manuscript results requires approved access to the original UNSW and UoM simulation datasets and the study-specific ancillary inputs. Public figures and aggregate results should be cleared by the study team before release.

## Structure

```text
ACEFA_GitHub_public/
├── run_analysis.R                     # Real-data workflow (private inputs needed)
├── run_demo.R                         # Synthetic demonstration, no private data
├── R/
│   ├── 00_setup.R                     # Libraries, scenario names, plotting setup
│   ├── 01_parameters.R                # Public base-case parameters and PSA distributions
│   ├── 02_data_preparation.R          # Load/merge transmission, life table, vaccination
│   ├── 03_cea_functions.R             # Original deterministic CEA functions, preserved
│   ├── 04_run_analysis.R              # Deterministic, economic-only and full-model PSA
│   ├── 05_results_tables.R            # Analysis tables
│   ├── 06_ceac_figures.R              # Memory-safe CEACs, including IQR selection
│   ├── 07_ce_plane_figures.R          # Full PSA CE plane
│   └── optional/                     # Tornado, 8-panel plane, two-way price,
│                                    # LAIV price threshold, scenario CEAC, crossings,
│                                    # and source-layout variants of the three CEAC figures
├── data_example/                      # Explicitly synthetic test data
├── data_private/                      # Ignored; place original UNSW/UoM datasets here
├── inputs/                            # Public non-transmission ancillary study data
├── outputs/                           # Ignored, requires review before release
├── tests/                             # Core invariants and release checks
└── .gitignore
```

## Getting started

Use R 4.1+ with packages: `haven`, `readxl`, `readr`, `dplyr`, `tidyr`, `purrr`, `tibble`, `ggplot2`, `scales`, and `data.table`.

Install missing packages in R:

```r
install.packages(c("haven", "readxl", "readr", "dplyr", "tidyr", "purrr",
                   "tibble", "ggplot2", "scales", "data.table"))
```

Start R / RStudio with the **repository root as working directory**. From the terminal:

```sh
Rscript run_demo.R
```

`run_demo.R` uses small PSA counts, creates synthetic CEACs and tables, and writes to the ignored `outputs/synthetic_demo/`. These results have **no scientific interpretation**.

For the full original-data analysis, add these four files (names are standardised from the original absolute file paths):

```text
data_private/UNSW_model_outputs.dta
data_private/UoM_model_outputs.dta
inputs/vaccine_allocation_table.csv
inputs/Australian Life Table.xlsx
```

Then run:

```sh
Rscript run_analysis.R
```

This defaults to **10,000 economic-only PSA iterations** and **1,000 full-model PSA draws**, using `set.seed(12345)` before each analysis, as in the source file. For a preliminary run with fewer draws, set environment variables `ACEFA_N_ECON` and `ACEFA_N_FULL` to positive integers. Restore the published counts before comparing results.

For the optional manuscript figure blocks, run all analyses in the same R session using:

```r
Sys.setenv(ACEFA_RUN_OPTIONAL_FIGURES = "1")
source("run_analysis.R")
```

The optional figure scripts preserve the most recent corresponding source blocks, with paths made relative; **they have not been independently run in this environment**. The full-model figures can be memory intensive.

## Analytical assumptions preserved from the original script

- Baseline strategy: `Baseline`; central LAIV strategies as defined in `R/00_setup.R`.
- Mutually exclusive non-hospitalised illness and admissions: `non_hosp_cases = pmax(disease - admissions, 0)` and `hosp_cases = admissions`.
- QALY losses: non-hospitalised cases × 0.009; hospitalisations × 0.031; mortality × age-specific discounted remaining life years × 0.86.
- Disease costs: non-hospitalised illness × $43.90; admissions × $10,385.40.
- Vaccine product: IIV $10/dose; LAIV $25/dose. School delivery $12.77/dose, other delivery $26.01/dose, additional under-five delivery $6/dose.
- Mortality life years discounted at 5%; epidemiological unit conventions are **not rescaled** beyond the supplied model input values.
- One economic parameter draw is applied to *all* models, intervention strategies and transmission-model simulations within each `psa_iter`. Baseline pairing uses `model_source` and `psa_iter` for economic-only PSA, plus `simulation_index` for full PSA.
- CEACs present $15,000/QALY as a dotted vertical line and $50,000/QALY as solid, without an x-axis tick at $15,000.

## Important validation requirement

These files were refactored from an uploaded ~18,000-line analysis script. Its two CEA functions were copied directly; other code was reorganised and reviewed statically. The original confidential datasets were **not provided**, and an R interpreter is not available in the authoring environment. Consequently no claim is made that the new pipeline has been numerically validated against the existing manuscript results.

Before publishing, run it locally on the approved inputs, compare **by model and scenario** the original and refactored results for total costs, QALY components, incremental QALYs, ICERs, CEACs and sensitivity analyses, confirm the sample sizes and strata, and review the outputs for confidentiality. Stochastic results should be checked using identical seeds, draw counts, software versions and RNG conventions.

## Use and citation

Add the final manuscript citation, contact information, author-approved licence, software versions / lockfile, and data-sharing statement before public GitHub release. No licence is assigned in this draft.

## Mapping from the original combined script

| Original script section | Public code |
|---|---|
| Sections 3–7: distributions and parameters | `R/01_parameters.R` |
| Sections 8–11: population, life table, allocation, means | `R/02_data_preparation.R` |
| Sections 13–14: full and mean-epidemiology CEA functions | `R/03_cea_functions.R` (copied verbatim) |
| Sections 15–19 and 31–33: base case, PSA, incrementals | `R/04_run_analysis.R` |
| Incremental and base-case tables | `R/05_results_tables.R` |
| Final CEACs and IQR transmission-model selection | `R/06_ceac_figures.R`; original plot layouts also in `R/optional/07–09` |
| Final full-model CE plane | `R/07_ce_plane_figures.R` |
| Final tornado, 8-panel CE plane, vaccine price analyses and scenario CEAC | `R/optional/01–06` |

The 18,078-line source included repeated superseded figures, intermediate diagnostics, experimental alternatives and whole-environment saves. Those were not copied wholesale. The optional code retains the most recent relevant figure-specific analyses, but it has not been validated on the original inputs. Some exploratory or superseded analysis variants are intentionally not part of this draft.
