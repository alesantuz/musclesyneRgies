# musclesyneRgies

The file "muscle_synergies.R" contains a script that allows to extract muscle synergies from electromyographic (EMG) data through linear decomposition based on machine learning. Specifically, here we adopted the non-negative matrix factorization (NMF) framework, due to the non-negative nature of EMG biosignals. However, this method can be applied to any other kind of data sets, from time series to images.

# Quick instruction to run the script
- [Download R](https://cran.r-project.org/mirrors.html) and install
- [Download RStudio](https://rstudio.com/products/rstudio/download/) and install
- Branch this project or, if you are new to GitHub, you can have a look at [this tutorial page](https://r-bio.github.io/intro-git-rstudio/) or manually download the script "muscle_synergies.R" and the two data sets "CYCLE_TIMES.RData" and "RAW_EMG.RData"
- Open "muscle_synergies.R" with RStudio
- Run the script with "Source" or "Ctrl+Shift+S" (Windows and Linux users) or "Cmd+Shift+S" (Mac users).

The code produces some diagnostic messages that will guide you through the process of:
- Raw EMG filtering and normalization (STEP 1/3)
- Muscle synergies extraction via NMF (STEP 2/3)
- Muscle synergies classification via NMF (STEP 3/3).

There will be plots, so make sure that the plot pane in RStudio is selected.
