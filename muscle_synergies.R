# The minimum requirements for successfully running this script are:
# 1. Raw data prepared in RData format, one list where each named element is a trial containing
#     i.  Raw EMG (RAW_DATA[[ii]]$emg) formatted as data frame or matrix with one column named
#         "time" (case insensitive) and other columns named as muscles
#     ii. Cycle times (RAW_DATA[[ii]]$cycles) formatted as data frame or matrix without specific
#         column names and as many columns as the number of phases each cycle should be divided
#         into after time-normalisation (e.g., for locomotion two columns, one with the timings
#         of all touchdowns and one with the timings of all the lift-offs, etc.)
# 2. R version 4+.

# # Preparation ----
# # Install (if needed) required packages
# pkgs_list <- c("pbapply",
#                # "tcltk",
#                # "parallel",
#                # "progress",
#                "signal",
#                "gtools",
#                "umap",
#                "Cairo",
#                "ggplot2",
#                "gridExtra",
#                "benchmarkme",
#                "plyr",
#                "reshape2")
#
# pkgs_new <- pkgs_list[!(pkgs_list %in% installed.packages()[, "Package"])]
# if (length(pkgs_new)) install.packages(pkgs_new)
#
# # Load required packages
# lapply(pkgs_list, library, character.only=T)
# # ATTENTION! The following line removes all objects from the global environment
# # except for the ones listed ("cl" in this case)
# rm(list=setdiff(ls(), c("cl")))

# Where are your files located if not in the same folder as the project's?
if (file.exists("RAW_DATA.RData")) {
  data_path <- getwd()
} else {
  if (.Platform$OS.type=="windows") {
    data_path <- choose.dir(caption="Select data folder")
    data_path <- paste0(gsub("\\\\", .Platform$file.sep, data_path), .Platform$file.sep)
  } else {
    data_path <- tcltk::tk_choose.dir(caption="Select data folder")
  }
}

# Create "Graphs" folder if it does not exist
data_path  <- paste0(data_path, .Platform$file.sep)
graph_path <- paste0(data_path, "Graphs", .Platform$file.sep)
dir.create(graph_path, showWarnings=F)

# If data are not prepared in RData format, please use the following code to read your ASCII files
# (place raw EMG and cycle timings in two different folders and then run the function "rawdata")
qq <- 1
while (!is.na(qq)) {
  message("\nDo you want to read data from ASCII files (type 'y' for 'yes', 'n' for 'no')?")
  qq <- readline()
  # Break if user decides
  if (qq=="y" || qq=="yes" || qq=="n" || qq=="no") break
}
if (qq=="y" || qq=="yes") musclesyneRgies::rawdata(header=TRUE)

# STEP 1 - Raw data processing ----
message("\n################################",
        "\n STEP 1/4 - Raw data processing",
        "\n################################")

test <- length(list.files(data_path, pattern="^FILT_EMG.RData$"))

if (test==1) {
  qq <- 1
  while (!is.na(qq)) {
    message("\nFiltered EMG data is already present in the specified folder!",
            "\nDo you want to use this data (type 'y' for 'yes') or recalculate (type 'n' for 'no')?",
            "\nNOTE: new calculations might require a few minutes, depending on the size of the data set")
    qq <- readline()
    # Break if user decides
    if (qq=="y" || qq=="yes" || qq=="n" || qq=="no") break
  }
} else if (test==0) qq <- "n"

if (qq=="n") {
  # Load raw EMG data and gait cycle times if not already done
  if (all(!grepl("^RAW_DATA$", objects()))) {
    message("\nLoading raw data...")
    load(paste0(data_path, "RAW_DATA.RData"))
    message("...done!")
  }

  # Create "Graphs/EMG" folder if it does not exist
  path_for_graphs <- paste0(graph_path, "EMG", .Platform$file.sep)
  dir.create(path_for_graphs, showWarnings=F)
  # Create "Graphs/EMG/raw" folder if it does not exist
  subfolder <- paste0(path_for_graphs, "raw", .Platform$file.sep)
  dir.create(subfolder, showWarnings=F)
  # Create "Graphs/EMG/filtered" folder if it does not exist
  subfolder <- paste0(path_for_graphs, "filtered", .Platform$file.sep)
  dir.create(subfolder, showWarnings=F)

  # Subset raw EMG
  message("\nSubsetting raw EMG...")
  RAW_DATA <- pbapply::pblapply(RAW_DATA, function(x) musclesyneRgies::subsetEMG(x,
                                                                cy_max=30,
                                                                cy_start=1))
  message("...done!")

  # Filter raw EMG
  message("\nFiltering raw EMG...")
  FILT_EMG <- pbapply::pblapply(RAW_DATA, function(x) musclesyneRgies::filtEMG(x,
                                                              HPf=50,
                                                              HPo=4,
                                                              LPf=20,
                                                              LPo=4))
  message("...done!")

  # Time-normalise filtered EMG
  message("\nTime-normalisng filtered EMG...")
  FILT_EMG <- pbapply::pblapply(FILT_EMG, function(x) musclesyneRgies::normEMG(x,
                                                              trim=TRUE,
                                                              cy_max=30,
                                                              cycle_div=c(100, 100)))
  message("...done!")

  # Plot "plot_time" seconds of raw EMG, starting from second "start"
  message("\nPlotting raw EMG...")
  pb <- pbapply::startpb(0, length(RAW_DATA))
  for (tt in seq_along(RAW_DATA)) {
    musclesyneRgies::plot_rawEMG(RAW_DATA[[tt]],
                trial=names(RAW_DATA)[tt],
                plot_time=3, start=1,
                path_for_graphs=paste0(path_for_graphs, "raw", .Platform$file.sep),
                filetype="png", width=2000, height=2500, resolution=280)
    pbapply::setpb(pb, tt)
  }
  pbapply::closepb(pb)
  message("...done!")

  # Plot mean cycles
  message("\nPlotting mean filtered and time-normalised EMG...")
  pb <- pbapply::startpb(0, length(FILT_EMG))
  for (tt in seq_along(FILT_EMG)) {
    musclesyneRgies::plot_meanEMG(FILT_EMG[[tt]],
                 trial=names(FILT_EMG)[tt],
                 path_for_graphs=paste0(path_for_graphs, "filtered", .Platform$file.sep),
                 filetype="png", width=2000, height=1875, resolution=140)
    pbapply::setpb(pb, tt)
  }
  pbapply::closepb(pb)
  message("...done!")

  message("\nSaving filtered EMG...")
  save(FILT_EMG, file=paste0(data_path, "FILT_EMG.RData"))
  message("...done!")
}

# STEP 2 - Synergies extraction ----
message("\n#################################",
        "\n STEP 2/4 - Synergies extraction",
        "\n#################################")

test <- length(list.files(data_path, pattern="^SYNS.RData$"))

if (test==1) {
  qq <- 1
  while (!is.na(qq)) {
    message("\nSynergy data is already present in the specified folder!",
            "\nDo you want to use this data (type 'y' for 'yes') or recalculate (type 'n' for 'no')?",
            "\nNOTE: new calculations might require a few hours, depending on the size of the data set")
    qq <- readline()
    # Break if user decides
    if (qq=="y" || qq=="yes" || qq=="n" || qq=="no") break
  }
} else if (test==0) qq <- "n"

if (qq=="n" || qq=="no") {
  # Load filtered EMG data if not already done
  if (all(!grepl("^FILT_EMG$", objects()))) {
    message("\nLoading filtered EMG...")
    load(paste0(data_path, "FILT_EMG.RData"))
    message("...done!")
  }
  ll <- length(FILT_EMG)

  # Get date and time for display before starting computation
  date <- gsub(" [0-9][0-9]:[0-9][0-9]:[0-9][0-9]$", "", Sys.time())
  time <- gsub("^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] ", "", Sys.time())

  message("\nExtract synergies\nStarted on ", date, " at ", time)

  tictoc <- system.time({
    # Create cluster for parallel computing if not already done
    clusters <- objects()

    if (sum(grepl("^cl$", clusters))==0) {
      # Decide how many processor threads have to be excluded from the cluster
      # It is a good idea to leave at least one free, so that the machine can be used during computation
      cl <- parallel::makeCluster(max(1, parallel::detectCores()-1))
    }
    # "synsNMF" is the core function for extracting synergies
    # and here it is applied in parallel to speed up computation
    SYNS <- pbapply::pblapply(FILT_EMG, musclesyneRgies::synsNMF, cl=cl)

    parallel::stopCluster(cl)
  })

  message("- - - - - - - - - - - - - - - - - - - - - - -\n",
          benchmarkme::get_sys_details()$data,
          "\n",
          benchmarkme::get_sys_details()$sys_info$sysname, " ",
          benchmarkme::get_sys_details()$sys_info$release, " ",
          benchmarkme::get_sys_details()$sys_info$version,
          "\n",
          benchmarkme::get_sys_details()$cpu$model_name, " ",
          "\n",
          R.version$version.string,
          "\n",
          parallel::detectCores(logical=F), " cores, ",
          parallel::detectCores(), " logical, ",
          length(cl), " used",
          "\n\n        Number of trials: ", ll,
          "\n        Computation time: ", round(tictoc[[3]], 0), " s",
          "\nAverage trial comp. time: ", round(tictoc[[3]]/ll, 2), " s\n",
          "\n- - - - - - - - - - - - - - - - - - - - - - -")

  message("\nSaving synergies...")
  save(SYNS, file=paste0(data_path, "SYNS.RData"))
  message("...done!")

  # Create "Graphs/NMF" folder if it does not exist
  path_for_graphs <- paste0(graph_path, "NMF", .Platform$file.sep)
  dir.create(path_for_graphs, showWarnings=F)
  # Create "Graphs/NMF/trials" folder if it does not exist
  path_for_graphs <- paste0(path_for_graphs, "trials", .Platform$file.sep)
  dir.create(path_for_graphs, showWarnings=F)

  # Plot muscle synergies (individual trials)
  max_syns <- max(unlist(lapply(lapply(SYNS, function(x) x$M), function(x) ncol(x))))
  message("\nPlotting muscle synergies (individual trials)...")
  pb <- pbapply::startpb(0, length(SYNS))
  for (tt in seq_along(SYNS)) {
    musclesyneRgies::plot_syn_trials(SYNS[[tt]],
                   max_syns=max_syns,
                   trial=names(SYNS)[tt],
                   path_for_graphs=path_for_graphs,
                   filetype="png", width=1800, height=2500, resolution=280)
    pbapply::setpb(pb, tt)
  }
  pbapply::closepb(pb)
  message("...done!")
}

# STEP 3 - Classification of muscle synergies ----
message("\n###############################################",
        "\n STEP 3/4 - Classification of muscle synergies",
        "\n###############################################")

test <- length(list.files(data_path, pattern="^SYNS_classified.RData$"))

if (test==1) {
  qq <- 1
  while (!is.na(qq)) {
    message("\nClassified synergy data is already present in the specified folder!",
            "\nDo you want to use this data (type 'y' for 'yes') or recalculate (type 'n' for 'no')?",
            "\nNOTE: new calculations might require a few minutes, depending on the size of the data set")
    qq <- readline()
    # Break if user decides
    if (qq=="n" || qq=="no") {
      # Prompt for classification method
      ww <- 1
      while (!is.na(ww)) {
        message("\nPlease choose a classification method: k-means (type 'k') or NMF (type 'n')",
                "\nNOTE: results might differ depending on the classification method")
        ww <- readline()
        # Break if user decides
        if (ww=="k" || ww=="n") break
      }
      break
    } else if (qq=="y" || qq=="yes") break
  }
} else if (test==0) {
  qq <- "n"
  # Prompt for classification method
  ww <- 1
  while (!is.na(ww)) {
    message("\nPlease choose a classification method: k-means (type 'k') or NMF (type 'n')",
            "\nNOTE: results might differ depending on the classification method")
    ww <- readline()
    # Break if user decides
    if (ww=="k" || ww=="n") break
  }
}

# Load data and define common functions
if (qq=="n" || qq=="no") {
  # Load muscle synergies if not already done
  if (all(!grepl("^SYNS$", objects()))) {
    load(paste0(data_path, "SYNS.RData"))
  }

  # Create "Graphs/NMF" folder if it does not exist
  path_for_graphs <- paste0(graph_path, "NMF", .Platform$file.sep)
  dir.create(path_for_graphs, showWarnings=F)
}

if ((qq=="n" || qq=="no") && ww=="k") {
  # Unsupervised learning method to classify synergies based on k-means
  SYNS_classified_TW <- musclesyneRgies::classify_kmeans(SYNS[grep("TW", names(SYNS))],
                                        path_for_graphs=path_for_graphs)
  SYNS_classified_TR <- musclesyneRgies::classify_kmeans(SYNS[grep("TR", names(SYNS))],
                                        path_for_graphs=path_for_graphs)

  SYNS_classified <- c(SYNS_classified_TW, SYNS_classified_TR)

  message("\nSaving classified synergies...")
  save(SYNS_classified, file=paste0(data_path, "SYNS_classified.RData"))
  message("...done!")

} else if ((qq=="n" || qq=="no") && ww=="n") {
  # Unsupervised learning method to classify synergies based on NMF
  SYNS_classified_TW <- musclesyneRgies::classify_NMF(SYNS[grep("TW", names(SYNS))],
                                     path_for_graphs=path_for_graphs)
  SYNS_classified_TR <- musclesyneRgies::classify_NMF(SYNS[grep("TR", names(SYNS))],
                                                      path_for_graphs=path_for_graphs)

  SYNS_classified <- c(SYNS_classified_TW, SYNS_classified_TR)

  message("\nSaving classified synergies...")
  save(SYNS_classified, file=paste0(data_path, "SYNS_classified.RData"))
  message("...done!")
}

# STEP 4 - Plot muscle synergies ----
message("\n##################################",
        "\n STEP 4/4 - Plot muscle synergies",
        "\n##################################")

if (all(!grepl("^SYNS_classified$", objects())) || all(!grepl("^FILT_EMG$", objects()))) {
  load(paste0(data_path, "SYNS_classified.RData"))
  load(paste0(data_path, "FILT_EMG.RData"))
}

# Create "Graphs/NMF" folder if it does not exist
path_for_graphs <- paste0(graph_path, "NMF", .Platform$file.sep)
dir.create(path_for_graphs, showWarnings=F)

# Plot 2D UMAP of average filtered and normalised EMG
musclesyneRgies::plot_EMG_UMAP(FILT_EMG[grep("TW", names(FILT_EMG))],
              path_for_graphs=paste0(graph_path, "EMG", .Platform$file.sep),
              condition="TW")

musclesyneRgies::plot_EMG_UMAP(FILT_EMG[grep("TR", names(FILT_EMG))],
              path_for_graphs=paste0(graph_path, "EMG", .Platform$file.sep),
              condition="TR")

# Plot 2D UMAP of synergies
musclesyneRgies::plot_syns_UMAP(SYNS_classified[grep("TW", names(SYNS_classified))],
              path_for_graphs=path_for_graphs,
              condition="TW")

musclesyneRgies::plot_syns_UMAP(SYNS_classified[grep("TR", names(SYNS_classified))],
               path_for_graphs=path_for_graphs,
               condition="TR")

# Plot muscle synergies
musclesyneRgies::plot_syns_all(SYNS_classified[grep("TW", names(SYNS_classified))],
               path_for_graphs=path_for_graphs,
               condition="TW")

musclesyneRgies::plot_syns_all(SYNS_classified[grep("TR", names(SYNS_classified))],
               path_for_graphs=path_for_graphs,
               condition="TR")
