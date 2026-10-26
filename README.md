# Microplastic particles recovered from millipedes

Reproducible analysis of recovered particle counts and mean particle sizes in individual millipedes. The complete experimental dataset is embedded in `R/data.R`; no data downloads or external input files are required.

The self-contained [HTML report](reports/analysis.html) presents Materials and Methods, the complete R code, statistical results, and 12 numbered figures with short captions. Its first analysis code block creates the dataset directly in R.

## Run the analysis

Open `Microplastics_Millipedes.Rproj` in RStudio or run these commands from the repository root:

```bash
Rscript scripts/install_packages.R
Rscript scripts/render_report.R
```

Rendering rebuilds the analysis, runs the validation checks, and writes `reports/analysis.html`. Alternatively, generate the results without rendering the report and then validate them:

```bash
Rscript scripts/run_analysis.R
Rscript scripts/check_analysis.R
```

Required R packages are `ggplot2`, `dplyr`, `patchwork`, `knitr`, and `rmarkdown`. HTML rendering also requires Pandoc, which is included with RStudio. The workflow was tested with R 4.6.1. The report records the actual R and package versions; run-specific details are also saved in `results/package_versions.csv` and `results/sessionInfo.txt`.

## Embedded dataset

`R/data.R` creates `input_data` using base R and records data-handling flags in `data_issues`. Each row represents one individual and one biological replicate. The dataset contains 275 individuals: 250 exposed individuals and 25 unexposed controls. The exposure design comprises five life forms, five polymers, two input-size fractions, and five individuals per combination. Each life form has five controls.

| Variable | Definition |
| --- | --- |
| `sample_id` | Unique individual identifier. |
| `life_form` | Millipede life-form category. |
| `polymer` | Polymer treatment or control. |
| `input_size_raw` | Recorded input-size label, retained for provenance. |
| `input_size_class` | Exposure size fraction in mm; missing for controls. |
| `is_control` | Logical indicator of an unexposed control. |
| `replicate_label` | Individual number within a treatment; does not imply pairing across treatments. |
| `output_size_raw` | Recorded mean recovered particle size in mm, including recorded zeros. |
| `output_count` | Number of recovered particles per individual. |
| `output_size_mm` | Mean recovered particle size in mm; defined only when count and recorded size are positive. |
| `input_lower_mm` | Lower bound of the input-size fraction; missing for controls. |
| `input_upper_mm` | Upper bound of the input-size fraction; missing for controls. |

All observed zero counts are retained, including 57 exposed individuals and all 50 platydesmoid individuals. All controls have zero counts. Mean size is defined for 193 exposed individuals; these individuals enter the size comparisons and PCA. Undefined sizes are not replaced with zero. Figures retain an indication of individuals without recovered particles.

## Statistical analysis

Pairwise permutation tests compare life forms within each polymer × size fraction, polymers within each life form × size fraction, and size fractions within each life form × polymer. Count effects are differences in arithmetic means; size effects are ratios of geometric means. Each individual receives equal weight. Benjamini–Hochberg adjustment is applied within each response × comparison family, with `p_BH < 0.05` used as the decision threshold. Approximate individual 95% Welch intervals describe effect uncertainty and are not adjusted for multiplicity.

Omnibus permutation tests compare life forms within each polymer × size fraction using 9,999 permutations, a plus-one correction, and separate Benjamini–Hochberg adjustment for each response. PCA uses standardized `log1p(count)` and `log(mean size)` and is descriptive. The random seed and analysis settings are defined in `config.R`.

Permutation inference assumes independent individuals and exchangeability under the null hypothesis. Life form is associated with species, so their effects cannot be separated. Recovered counts and sizes alone do not establish fragmentation efficiency or degradation rates.

## Repository contents

- `R/data.R`: complete embedded dataset and data-handling flags.
- `R/analysis.R` and `R/inference.R`: summaries, permutation tests, effect estimates, and PCA.
- `R/figures.R` and `R/plot_theme.R`: publication figures and visual settings.
- `R/report_helpers.R`: source-code display and table formatting.
- `reports/analysis.Rmd`, `reports/report.css`, and `reports/analysis.html`: report source, styling, and self-contained output.
- `scripts/`: dependency installation, analysis, rendering, and validation entry points.

Generated analysis tables are written as CSV files to `results/tables/`; figures are saved as PNG and PDF files in `results/figures/`. These reproducible outputs are excluded from Git. The embedded data, analysis code, and rendered HTML report are included in Git.

## Publish on GitHub

Create an empty GitHub repository, then run the following commands from this repository root, replacing the URL with your repository address:

```bash
git remote add origin <YOUR_GITHUB_REPOSITORY_URL>
git push -u origin main
```
