# musclesyneRgies

The file "muscle_synergies.R" contains a script that allows to extract muscle synergies from electromyographic (EMG) data through linear decomposition based on machine learning. Specifically, here we adopted the non-negative matrix factorization (NMF) framework, due to the non-negative nature of EMG biosignals. However, this method can be applied to any other kind of data sets, from time series to images.

# Quick instructions to run the script
- [Download R](https://cran.r-project.org/mirrors.html) and install
- [Download RStudio](https://rstudio.com/products/rstudio/download/) and install
- [Download Git](https://git-scm.com/downloads) and install
- Branch this project or, if you are new to GitHub, you can have a look at [this tutorial page](https://r-bio.github.io/intro-git-rstudio/) or manually download the repository
- Open the project file "musclesyneRgies.Rproj" with RStudio
- From within the project, open the script "muscle_synergies.R" and run it with "Source" or "Ctrl+Shift+S" (Windows and Linux users) or "Cmd+Shift+S" (Mac users).

The code produces some diagnostic messages that will guide you through the process of:
- Raw EMG filtering and normalization (STEP 1/4)
- Muscle synergies extraction via NMF (STEP 2/4)
- Muscle synergies classification via NMF (STEP 3/4)
- Plotting the classified synergies (STEP 4/4).

There will be plots within RStudio in the 4th step, so make sure that the plot pane in RStudio is selected.

# References
1. Mileti, I. et al. [Muscle activation patterns are more constrained and regular in treadmill than in overground human locomotion](https://www.frontiersin.org/articles/10.3389/fbioe.2020.581619/full). Front. Bioeng. Biotechnol. (2020).
2. Santuz, A. et al. [Lower complexity of motor primitives ensures robust control of high-speed human locomotion](https://www.biorxiv.org/content/10.1101/2020.04.24.055277v1). bioRxiv (2020).
3. Santuz, A. et al. [Neuromotor Dynamics of Human Locomotion in Challenging Settings](https://www.cell.com/iscience/fulltext/S2589-0042(19)30542-5). iScience 23, 100796 (2020).
4. Santuz, A. et al. [Modular organization of murine locomotor pattern in the presence and absence of sensory feedback from muscle spindles](https://physoc.onlinelibrary.wiley.com/doi/abs/10.1113/JP277515). J. Physiol. 597, 3147–3165 (2019).
5. Santuz, A. et al. [Modular Control of Human Movement During Running: An Open Access Data Set](https://www.frontiersin.org/articles/10.3389/fphys.2018.01509/full). Front. Physiol. 9, 1509 (2018).
6. Santuz, A., Ekizos, A., Eckardt, N., Kibele, A. & Arampatzis, A. [Challenging human locomotion: stability and modular organisation in unsteady conditions](https://www.nature.com/articles/s41598-018-21018-4). Sci. Rep. 8, 2740 (2018).
7. Santuz, A., Ekizos, A., Janshen, L., Baltzopoulos, V. & Arampatzis, A. [The Influence of Footwear on the Modular Organization of Running](https://www.frontiersin.org/articles/10.3389/fphys.2017.00958/full). Front. Physiol. 8, 958 (2017).
8. Santuz, A., Ekizos, A., Janshen, L., Baltzopoulos, V. & Arampatzis, A. [On the Methodological Implications of Extracting Muscle Synergies from Human Locomotion](https://www.worldscientific.com/doi/abs/10.1142/S0129065717500071). Int. J. Neural Syst. 27, 1750007 (2017).
