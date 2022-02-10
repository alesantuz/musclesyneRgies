# musclesyneRgies 1.1.3

### How to install
```
install.packages("musclesyneRgies")
```

### How to use
README and vignettes are available both on [CRAN](https://CRAN.R-project.org/package=musclesyneRgies) and on [GitHub](https://github.com/alesantuz/musclesyneRgies).

### What's changed
- Faster examples and tests for CRAN
- Removed `classify_NMF` function.

# musclesyneRgies 1.1.2
### How to install
```
install.packages("musclesyneRgies")
```

### How to use
README and vignettes are available both on [CRAN](https://CRAN.R-project.org/package=musclesyneRgies) and on [GitHub](https://github.com/alesantuz/musclesyneRgies).

### What's changed
- Removed all progress bars.

# musclesyneRgies 1.1.1

### How to install
```
install.packages("musclesyneRgies")
```

### How to use
README and vignettes are available both on [CRAN](https://CRAN.R-project.org/package=musclesyneRgies) and on [GitHub](https://github.com/alesantuz/musclesyneRgies).

### What's changed
- `musclesyneRgies` is on CRAN!
- Adjusted `test-CoA.R` to pass on `M1mac` and `noLD`.

# musclesyneRgies 1.1.0

### How to install
```
install.packages("devtools")
library(devtools)
install_github("alesantuz/musclesyneRgies")
```
### How to use
Follow the [README](https://github.com/alesantuz/musclesyneRgies/blob/master/README.md) for a breakdown of the typical workflow.

### What's changed
- This version was submitted to CRAN for checks
- Added tests
- Added `NEWS.md` to track changes to the package
- Function `plot_rawEMG` can now plot muscle activities in custom grid (arguments `row_number` and `col_number`)
- Line thickness and colour can now be specified in function `plot_rawEMG` (arguments `line_size` and `line_col`)
- Added dark mode to `plot_rawEMG`, `plot_meanEMG`, `plot_syn_trials` and `plot_classified_syns`
- Function `plot_syn_trials` partially rewritten with new native pipes `|>` and `lapply` instead of for loops
- Updated `README`
- Added `CITATION.cff` using R package `cffr`
- Simplified all plot functions, which now exports `gtable` objects rather tan saving to file
- Added tests for plot functions
- Added vignettes (`workflow`, `plots`, `analysis` and `pro_tips`).

# musclesyneRgies 1.0.1

### How to install
```
install.packages("devtools")
library(devtools)
install_github("alesantuz/musclesyneRgies")
```
### How to use
Follow the [README](https://github.com/alesantuz/musclesyneRgies/blob/master/README.md) for a breakdown of the typical workflow.

### What's changed
- Rectification can now be avoided in `filtEMG` if needed with `rectif = "none"`
- Functions `plot_rawEMG` and `plot_meanEMG` partially rewritten with new native pipes `|>` and `lapply` instead of for loops
- Fixed a bug in `synsNMF` that didn't L2-normalise `M` and `P` in iteration "zero".

# musclesyneRgies 1.0.0

### How to install
```
install.packages("devtools")
library(devtools)
install_github("alesantuz/musclesyneRgies")
```
### What's changed
- The short-term maximum Lyapunov exponents of motor primitives can now be calculated with the new function `sMLE`
- Zero- or negative values in relevant matrices are now replaced with the smallest non-zero around twice as fast
- L2-normalisation within `synsNMF` is now 1.5-2x faster
- All code re-styled in [tidyverse style](https://style.tidyverse.org/) using [styler](https://styler.r-lib.org/)
- Fixed a bug in `filtEMG` that didn't allow to avoid low-pass filtering if needed.

# musclesyneRgies 0.9.4-beta

- All code re-styled with tidyverse style using styler, plus new logo!

v1.0.0 coming soon with function for calculating the short-term maximum Lyapunov exponents of motor primitives.

# musclesyneRgies 0.9.3-beta

- Improved robustness of functions `subsetEMG` and `normEMG` when cycle times are given as data frames.

# musclesyneRgies 0.9.2-beta

- This not-so-minor update allows the user to extract synergies with a fixed rank.

# musclesyneRgies 0.9.1-beta

- `musclesyneRgies` is now a R package. Follow the README for a breakdown of the typical workflow. This is the first release, still in beta.

# musclesyneRgies 0.8.7-alpha

- Fixed a bug in the UMAP plots.

# musclesyneRgies 0.8.6-alpha

- Housekeeping and UMAP plots of mean filtered EMG.

# musclesyneRgies 0.8.5-alpha

- UMAP plots of classified synergies are saved in the NMF folder for advanced visualisation of factorisation/classification outcomes.

UMAP details:
McInnes, Leland, John Healy, and James Melville. "Umap: Uniform manifold approximation and projection for dimension reduction." arXiv preprint arXiv:1802.03426 (2018).

R-package:
https://cran.r-project.org/web/packages/umap/

# musclesyneRgies 0.8.4-alpha

- Raw EMG plots are now saved for each trial. The last `raw_pl` (see line 133) seconds of the trial are considered for plots.

# musclesyneRgies 0.8.3-alpha

- When reordering synergies manually during classification, the user can now repeat endlessly the classification if mistakes are made during input.

# musclesyneRgies 0.8.2-alpha

- A bit of housekeeping and now the clusters found by k-means classification are saved in one plot per condition.

# musclesyneRgies 0.8.1-alpha

- The robustness of k-means classification is improved and can now better deal with special cases of clusters with similar CoA and FWHM.

# musclesyneRgies 0.7.3-alpha

- Now the classification method is saved in the SYNS_classified list and can be used by step 4 (graph creation) to save the classification method in the file name.

# musclesyneRgies 0.7.2-alpha

- Now the classification method is saved in the file name when producing graph in step 4.

# musclesyneRgies 0.7.1-alpha

- First public API of `musclesyneRgies`
- Fixed a bug in section 4
