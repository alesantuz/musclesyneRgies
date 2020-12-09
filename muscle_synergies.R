# The minimum requirements for successfully running this script are:
# 1. Raw EMG data prepared in RData format, one list where each named element is a trial
#    formatted as data frame with columns named "time" and muscle abbreviations,
#    file saved as "RAW_EMG.RData"
# 2. Gait cycle times prepared in RData format, one list where each named element is a trial
#    formatted as data frame with columns named "touchdown" and "stance", times in [s],
#    file saved as "CYCLE_TIMES.RData"
# 3. Trials are named as "CYCLE_TIMES_Pxxxx_AA.*_yy" or "RAW_EMG_Pxxxx_AA.*_yy", where:
#    - Pxxxx is the participant number (e.g., P0002 or P0456)
#    - AA is the condition (e.g., TW or OR for treadmill or overground walking, but can
#      as well be longer such as TR_YOUNG_FEMALE, etc.)
#    - yy is the trial number (e.g., 01 or 155, etc.)
# Using regex: CYCLE_TIMES_P[0-9]*_.*_[0-9]*, RAW_EMG_P[0-9]*_.*_[0-9]*

# Preparation ----
# Install (if needed) required packages
pkgs_list <- c("tcltk",
               "parallel",
               "progress",
               "signal",
               "Cairo",
               "ggplot2",
               "gridExtra",
               "benchmarkme",
               "plyr",
               "reshape2")

pkgs_new <- pkgs_list[!(pkgs_list %in% installed.packages()[, "Package"])]
if (length(pkgs_new)) install.packages(pkgs_new)

# Load required packages
lapply(pkgs_list, library, character.only=T)
# ATTENTION! The following line removes all objects from the global environment
# except for the ones listed ("cl" in this case)
rm(list=setdiff(ls(), c("cl")))

# Where are your files located if not in the same folder as the project's?
if (all(file.exists("CYCLE_TIMES.RData", "RAW_EMG.RData"))) {
    data_path <- getwd()
} else {
    if (.Platform$OS.type=="windows") {
        data_path <- choose.dir(caption="Select data folder")
        data_path <- gsub("\\\\", .Platform$file.sep, data_path)
    } else {
        data_path <- tcltk::tk_choose.dir(caption="Select data folder")
    }
}

# Create "Graphs" folder if it does not exist
data_path  <- paste0(data_path, .Platform$file.sep)
graph_path <- paste0(data_path, "Graphs", .Platform$file.sep)
dir.create(graph_path, showWarnings=F)

# Create cluster for parallel computing if not already done
clusters <- objects()

if (sum(grepl("^cl$", clusters))==0) {
    # Decide how many processor threads have to be excluded from the cluster
    # It is a good idea to leave at least one free, so that the machine can be used during computation
    cl <- parallel::makeCluster(max(1, parallel::detectCores()-1))
}

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
        if (qq=="y" || qq=="n") break
    }
} else if (test==0) qq <- "n"

if (qq=="n") {
    # Load raw EMG data and gait cycle times if not already done
    if (all(!grepl("^CYCLE_TIMES$", objects())) && all(!grepl("^RAW_EMG$", objects()))) {
        message("\nLoading raw data...")
        load(paste0(data_path, "CYCLE_TIMES.RData"))
        load(paste0(data_path, "RAW_EMG.RData"))
        message("...done!")
    }
    
    # Create "Graphs/EMG" folder if it does not exist
    path_for_graphs <- paste0(graph_path, "EMG", .Platform$file.sep)
    dir.create(path_for_graphs, showWarnings=F)
    
    # Global filter and normalisation parameters, change as needed
    HPo    <- 4             # High-pass filter order
    HPf    <- 50            # High-pass filter frequency [Hz]
    LPo    <- HPo           # Low-pass filter order
    LPf    <- 20            # Low-pass filter frequency [Hz]
    points <- 200           # Gait cycle length (interpolated points)
    cy_max <- 30            # Max number of cycles to be analysed
    cycles <- numeric()     # To save number of cycles considered
    
    # Preallocate to write results
    FILT_EMG   <- vector("list", length(RAW_EMG))
    list_names <- character(length=length(RAW_EMG))
    
    # Progress bar
    pb <- progress::progress_bar$new(format="[:bar]:percent ETA: :eta",
                                     total=length(RAW_EMG), clear=F, width=50)
    
    message("\nApply filters")
    
    for (ii in seq_along(RAW_EMG)) {
        
        pb$tick()
        
        if (nrow(RAW_EMG[[ii]])<1) {
            stop("\n\nTrial ", ii, " (", trial, ") is empty! Please check your raw data.")
        }
        
        trial   <- gsub("RAW_EMG_", "", names(RAW_EMG[ii]))
        muscles <- names(RAW_EMG[[ii]])
        
        # Trim to the first cy_max+2 cycles
        # (+2 because first and last will be trimmed after filtering)
        c_time <- CYCLE_TIMES[[grep(trial, names(CYCLE_TIMES))]]
        trim   <- c_time$touchdown[cy_max+2]
        
        # Check if there are more than cy_max cycles and do not trim if false
        label <- which(RAW_EMG[[ii]]$time>trim)[1]
        
        if (is.na(label)) {
            emg_data <- RAW_EMG[[ii]]
        } else {
            emg_data <- RAW_EMG[[ii]][1:(label-1), ]
        }
        
        # Demean EMG (subtract mean value from the signal to eliminate offset shifts)
        time     <- emg_data$time
        emg_data <- apply(emg_data, 2, function(x) x-mean(x, na.rm=T))
        emg_data[, "time"] <- time
        
        # EMG system acquisition frequency [Hz]
        freq <- round(1/(mean(diff(emg_data[, "time"]), na.rm=T)), 0)
        
        # Filtering
        emg_data_filt <- emg_data
        emg_data_filt <- emg_data_filt[, colnames(emg_data_filt)!="time"]
        
        # High-pass IIR (Infinite Impulse Response) Butterworth zero-phase filter design
        # Critical frequencies must be between 0 and 1, where 1 is the Nyquist frequency
        # "filtfilt" is for zero-phase filtering
        HPfn <- HPf/(freq/2)                            # Normalise by the Nyquist frequency (f/2)
        HP   <- signal::butter(HPo, HPfn, type="high")
        emg_data_filt <- apply(emg_data_filt, 2, function(x) signal::filtfilt(HP, x))
        
        # Full-wave rectification
        emg_data_filt <- abs(emg_data_filt)
        
        # Low-pass IIR (Infinite Impulse Response) Butterworth zero-phase filter design
        # Critical frequencies must be between 0 and 1, where 1 is the Nyquist frequency
        # "filtfilt" is for zero-phase filtering
        LPfn <- LPf/(freq/2)                            # Normalise by the Nyquist frequency (f/2)
        LP   <- signal::butter(LPo, LPfn, type="low")
        emg_data_filt <- apply(emg_data_filt, 2, function(x) signal::filtfilt(LP, x))
        
        emg_data_filt[emg_data_filt<0] <- 0             # Set negative values to zero
        temp <- emg_data_filt
        temp[temp==0] <- Inf
        emg_data_filt[emg_data_filt==0] <- min(temp)    # Set the zeros to the smallest non-zero entry
        
        # Subtract the minimum
        emg_data_filt <- apply(emg_data_filt, 2, function(x) x-min(x))
        # Amplitude normalisation to the maximum of the trial
        emg_data_filt <- apply(emg_data_filt, 2, function(x) x/max(x))
        
        emg_time <- seq(emg_data[, "time"][1], emg_data[, "time"][nrow(emg_data_filt)], 1/freq)
        
        # Trim first and last cycle to remove filtering effects
        c_time <- c_time[2:(nrow(c_time)-1), ]
        cycs <- nrow(c_time)-1
        # Remove excess cycles, if present
        if (cycs>cy_max) cycs <- cy_max
        
        # Isolate cycles and normalise time to "points" points
        # (first half stance, second half swing)
        for (jj in 1:cycs) {
            # Stance
            temp <- data.frame()
            t1   <- c_time$touchdown[jj]
            t2   <- c_time$touchdown[jj]+c_time$stance[jj]
            
            if (t1>max(emg_time, na.rm=T) || t2>max(emg_time, na.rm=T)) {
                cycs <- jj-1
                break
            } else {
                t1   <- which(emg_time>=t1)[1]
                t2   <- which(emg_time>=t2)[1]
                temp <- emg_data_filt[t1:t2, ]
            }
            
            # Check if there is data
            if (sum(temp, na.rm=T)==0) next
            
            # Interpolate each channel to (points/2) points
            temp1 <- data.frame(time=c(1:(points/2)),
                                apply(temp, 2, function(x) approx(x,
                                                                  method="linear",
                                                                  n=points/2)$y))
            
            # Swing
            temp <- data.frame()
            t1   <- c_time$touchdown[jj]+c_time$stance[jj]
            t2   <- c_time$touchdown[jj+1]
            
            if (t1>max(emg_time, na.rm=T) || t2>max(emg_time, na.rm=T)) {
                cycs <- jj-1
                break
            } else {
                t1   <- which(emg_time>=t1)[1]
                t2   <- which(emg_time>=t2)[1]
                temp <- emg_data_filt[t1:t2, ]
            }
            
            # Check if there is data
            if (sum(temp, na.rm=T)==0) next
            
            # Interpolate each channel to (points/2) points
            temp2 <- data.frame(time=c(1:(points/2)),
                                apply(temp, 2, function(x) approx(x,
                                                                  method="linear",
                                                                  n=points/2)$y))
            
            temp <- rbind(temp1, temp2)
            
            # Set every value >1 to 1
            temp[temp>1] <- 1
            temp$time    <- c(1:points)
            
            # For the concatenated data
            if (jj==1) {
                emg_data_co <- temp
                
                # For the averaged data
                emg_data_av <- matrix(0, nrow=points, ncol=ncol(emg_data_co))
            } else {
                emg_data_co <- rbind(emg_data_co, temp)
                
                # For the averaged data
                emg_data_av <- emg_data_av+temp
            }
        }
        
        FILT_EMG[[ii]] <- emg_data_co
        list_names[ii] <- paste0("FILT_EMG_", trial)
        
        # Export average cycles for checking
        # Normalise the averaged data
        emg_data_av <- data.frame(apply(emg_data_av, 2, function(x) x/max(x)))
        emg_data_av[, "time"] <- c(1:points)
        
        # Make graphs
        Cairo::Cairo(file=paste0(path_for_graphs, "temp.png", sep=""),
                     width=1500, height=1500, pointsize=12, dpi=120)
        varlist <- list()
        
        for (mm in 2:length(muscles)) {
            data <- data.frame(emg_data_av$time,
                               emg_data_av[, grep(paste0("^", muscles[mm], "$"), colnames(emg_data_av))])
            colnames(data) <- c("time", "signal")
            
            varname <- paste("pp", mm, sep = "")
            
            temp <- ggplot2::ggplot() +
                ggplot2::ggtitle(muscles[mm]) +
                ggplot2::ylim(0, 1) +
                ggplot2::geom_line(data=data,
                                   ggplot2::aes(x=time,  y=signal),
                                   colour="black", size=0.9) +
                ggplot2::theme(axis.title=ggplot2::element_blank(),
                               panel.background=ggplot2::element_rect(fill="white", colour="gray"),
                               panel.grid.major=ggplot2::element_line(colour="gray", size=0.05),
                               panel.grid.minor=ggplot2::element_blank(),
                               legend.position = "none")
            
            varlist[[mm-1]] <- assign(varname, temp)
        }
        
        gridExtra::grid.arrange(grobs=varlist,
                                nrow=ceiling(sqrt(length(varlist))),
                                ncol=ceiling(sqrt(length(varlist))),
                                top=(paste0(trial, sep="")))
        
        dev.off() # Close Cairo export
        
        file.rename(paste0(path_for_graphs, "temp.png"),
                    paste0(path_for_graphs, "EMG_average_", trial, ".png", sep=""))
    }
    
    names(FILT_EMG) <- list_names
    
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
        if (qq=="y" || qq=="n") break
    }
} else if (test==0) qq <- "n"

if (qq=="n") {
    # Load filtered EMG data if not already done
    if (all(!grepl("^FILT_EMG$", objects()))) {
        message("\nLoading filtered EMG...")
        load(paste0(data_path, "FILT_EMG.RData"))
        message("...done!")
    }
    ll <- length(FILT_EMG)
    
    # Define non-negative matrix factorisation function
    synsNMFn <- function(V)
    {
        R2_target <- 0.01               # Convergence criterion (percent of the R2 value)
        R2_cross  <- numeric()          # R2 values for cross validation and syn number assessment
        M_list    <- list()             # To save factorisation M matrices (synergies)
        P_list    <- list()             # To save factorisation P matrices (primitives)
        Vr_list   <- list()             # To save factorisation Vr matrices (reconstructed signals)
        iters     <- numeric()          # To save the iterations number
        
        # Original matrix
        time   <- V$time
        V$time <- NULL
        V      <- as.matrix(t(V))       # Needs to be transposed for NMF
        V[V<0] <- 0                     # Set negative values to zero
        temp   <- V
        temp[temp==0] <- Inf
        V[V==0] <- min(temp, na.rm=T)   # Set the zeros to the smallest non-zero entry in V
        
        m <- nrow(V)                    # Number of muscles
        n <- ncol(V)                    # Number of time points
        
        max_syns <- m-round(m/4, 0)     # Max number of syns
        
        for (r in 1:max_syns) {         # Run NMF with different initial conditions
            R2_choice <- numeric()      # Collect the R2 values for each syn and choose the max
            
            # Preallocate to then choose those with highest R2
            M_temp  <- list()
            P_temp  <- list()
            Vr_temp <- list()
            
            for (j in 1:5) {            # Run NMF 5 times for each syn and choose best run
                # To save error values
                R2  <- numeric()        # 1st cost function (R squared)
                SST <- numeric()        # Total sum of squares
                RSS <- numeric()        # Residual sum of squares or min reconstruction error
                
                # Initialise iterations and define max number of iterations
                iter     <- 1
                max_iter <- 1000
                # Initialise the two factorisation matrices with random values (uniform distribution)
                P <- matrix(runif(r*n, min=0.01, max=1), nrow=r, ncol=n)
                M <- matrix(runif(m*r, min=0.01, max=1), nrow=m, ncol=r)
                
                # Iteration zero
                P   <- P*(t(M)%*%V)/(t(M)%*%M%*%P)
                M   <- M*(V%*%t(P))/(M%*%P%*%t(P))
                Vr  <- M%*%P          # Reconstructed matrix
                RSS <- sum((V-Vr)^2)
                SST <- sum((V-mean(V))^2)
                R2[iter] <- 1-(RSS/SST)
                
                # l2-norm normalisation which eliminates trivial scale indeterminacies
                # The cost function doesn't change. Impose ||M||2 = 1 and normalise P accordingly.
                # ||M||2, also called L2,1 norm or l2-norm, is a sum of Euclidean norm of columns.
                for (kk in 1:r) {
                    norm    <- sqrt(sum(M[, kk]^2))
                    M[, kk] <- M[, kk]/norm
                    P[kk, ] <- P[kk, ]*norm
                }
                
                # Start iterations for NMF convergence
                for (iter in iter:max_iter)  {
                    P   <- P*(t(M)%*%V)/(t(M)%*%M%*%P)
                    M   <- M*(V%*%t(P))/(M%*%P%*%t(P))
                    Vr  <- M%*%P
                    RSS <- sum((V-Vr)^2)
                    SST <- sum((V-mean(V))^2)
                    R2[iter] <- 1-(RSS/SST)
                    
                    # l2-norm normalisation
                    for (kk in 1:r) {
                        norm    <- sqrt(sum(M[, kk]^2))
                        M[, kk] <- M[, kk]/norm
                        P[kk, ] <- P[kk, ]*norm
                    }
                    
                    # Check if the increase of R2 in the last 20 iterations is less than the target
                    if (iter>20) {
                        R2_diff <- R2[iter]-R2[iter-20]
                        if (R2_diff<R2[iter]*R2_target/100) {
                            break
                        }
                    }
                }
                R2_choice[j] <- R2[iter]
                
                M_temp[[j]]  <- M
                P_temp[[j]]  <- P
                Vr_temp[[j]] <- Vr
            }
            
            choice <- which.max(R2_choice)
            
            R2_cross[r]  <- R2_choice[choice]
            M_list[[r]]  <- M_temp[[choice]]
            P_list[[r]]  <- P_temp[[choice]]
            Vr_list[[r]] <- Vr_temp[[choice]]
            iters[r]     <- iter
        }
        
        # Choose the minimum number of synergies using the R2 criterion
        MSE  <- 100                     # Initialise the Mean Squared Error (MSE)
        iter <- 0                       # Initialise iterations
        while (MSE>1e-04) {
            iter <- iter+1
            if (iter==r-1) {
                break
            }
            R2_interp <- as.data.frame(cbind(c(1:(r-iter+1)), R2_cross[iter:r]))
            colnames(R2_interp) <- c("synergies", "R2_values")
            
            nn      <- nrow(R2_interp)
            lin     <- lm(R2_values~synergies, R2_interp)
            lin_pts <- lin[[5]]
            MSE     <- sum((lin_pts-R2_interp[, 2])^2)/nn
        }
        syns_R2 <- iter
        
        P_choice <- data.frame(time, t(P_list[[syns_R2]]))
        colnames(P_choice) <- c("time", paste0("Syn", 1:(ncol(P_choice)-1)))
        rownames(P_choice) <- NULL
        
        return(list(synsR2=as.numeric(syns_R2),
                    M=M_list[[syns_R2]],
                    P=P_choice,
                    Vr=Vr_list[[syns_R2]],
                    iterations=as.numeric(iters[syns_R2]),
                    R2=as.numeric(R2_cross[syns_R2])))
    }
    
    # Get date and time for display before starting computation
    date <- gsub(" [0-9][0-9]:[0-9][0-9]:[0-9][0-9]$", "", Sys.time())
    time <- gsub("^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] ", "", Sys.time())
    
    message("\nExtract synergies\nStarted on ", date, " at ", time)
    
    tictoc <- system.time({
        # "synsNMFn" is the core function for extracting synergies
        # and here it is applied in parallel to speed up computation
        # At the moment there is no progress bar for parLapply
        SYNS <- parallel::parLapply(cl, FILT_EMG, synsNMFn)
    })
    
    names(SYNS) <- gsub("FILT_EMG", "SYNS", names(SYNS))
    
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
        if (qq=="y" || qq=="n") break
    }
} else if (test==0) qq <- "n"

if (qq=="n") {
    # Unsupervised learning method to classify synergies
    # Load muscle synergies if not already done
    if (all(!grepl("^SYNS$", objects()))) {
        load(paste0(data_path, "SYNS.RData"))
    }
    
    # Get concatenated primitives
    SYNS_P <- lapply(SYNS, function(x) x$P)
    
    # Make sure that all motor primitives are normalized to the same amount of points
    points <- unlist(lapply(SYNS_P, function(x) max(x$time)))
    
    if (sd(points)!=0) {
        message("\nNot all motor primitives are normalised to the same amount of points!",
                "\nPlease re-check your data\n")
    } else points <- unique(points)
    
    message("\nCalculating mean motor primitives...")
    
    # Progress bar
    pb <- progress::progress_bar$new(format="[:bar]:percent ETA: :eta",
                                     total=length(SYNS_P), clear=F, width=50)
    
    SYNS_P <- lapply(SYNS_P, function(x) {
        
        pb$tick()
        
        x$time <- NULL
        temp   <- matrix(0, nrow=points, ncol=ncol(x))
        
        for (cc in seq(1, (1+nrow(x)-points), points)) {
            temp <- temp+x[c(cc:(cc+points-1)), ]
        }
        
        # Divide by the number of cycles to get mean value
        temp <- temp/(nrow(x)/points)
        
        # Amplitude normalisation
        x <- apply(temp, 2, function(y) y/(max(y)))
        
        # Transpose to facilitate visualisation
        return(t(x))
    })
    message("...done!")
    
    message("\nPutting primitives in a single data frame...")
    
    # Progress bar
    pb <- progress::progress_bar$new(format="[:bar]:percent ETA: :eta",
                                     total=length(SYNS_P), clear=F, width=50)
    
    data <- plyr::ldply(SYNS_P, function(x) {
        pb$tick()
        data.frame(x)
    })
    message("...done!")
    
    # Give names to primitives (start from synergy zero because
    # the function "make.unique" works like that)
    # Find non-duplicated names and assign "Syn0" to them
    syn0           <- which(!duplicated(data$.id))
    data$.id       <- make.unique(data$.id)
    data$.id[syn0] <- paste0(data$.id[syn0], "_Syn0")
    # Assign incremental Syn number to other names
    data$.id <- gsub("\\.", "_Syn", data$.id)
    # Start from Syn1 instead that from Syn0
    temp1 <- gsub("[0-9]$", "", data$.id)
    temp2 <- as.numeric(gsub(".*_Syn", "", data$.id))+1
    temp3 <- paste0(temp1, temp2)
    # Assign new names to row names and remove id column
    rownames(data) <- temp3
    data$.id       <- NULL
    
    # Filter primitives to improve classification
    data <- t(apply(data, 1, function(x) {
        
        # Build filter
        LP <- signal::butter(4, 10/(points/2), type="low")
        # Apply filter
        x  <- signal::filtfilt(LP, t(x))
        
        # Remove negative entries
        x[x<0] <- 0
        # Subtract the minimum
        x <- x-min(x)
        # Set zeroes to smallest non-negative entry
        temp <- x
        temp[temp==0] <- Inf
        x[x==0] <- min(temp, na.rm=T)
        # Normalise to maximum
        x <- x/max(x)
        
        return(x)
    }))
    
    # Classify primitives with NMF
    # Define NMF function
    NMFn <- function(V)
    {
        R2_target <- 0.01               # Convergence criterion (percent of the R2 value)
        R2_cross  <- numeric()          # R2 values for cross validation and syn number assessment
        M_list    <- list()             # To save factorisation M matrices (synergies)
        P_list    <- list()             # To save factorisation P matrices (primitives)
        
        # Original matrix
        V      <- as.matrix(V)
        V[V<0] <- 0                     # Set negative values to zero
        temp   <- V
        temp[temp==0] <- Inf
        V[V==0] <- min(temp, na.rm=T)   # Set the zeros to the smallest non-zero entry in V
        
        m <- nrow(V)                    # Number of primitives in the data-set
        n <- ncol(V)                    # Number of time points
        
        # Determine the maximum number of synergies by searching for the maximum rank
        temp <- as.numeric(gsub(".*\\_Syn", "", rownames(V)))
        # Add one because interpolation must happen with at least two points
        max_syns <- max(temp)+1
        
        for (r in 1:max_syns) {         # Run NMF with different initial conditions
            R2_choice <- numeric()      # Collect the R2 values for each syn and choose the max
            
            # Preallocate to then choose those with highest R2
            M_temp <- list()
            P_temp <- list()
            
            for (j in 1:5) {            # Run NMF 5 times for each syn and choose best run
                # To save error values
                R2  <- numeric()        # 1st cost function (R squared)
                SST <- numeric()        # Total sum of squares
                RSS <- numeric()        # Residual sum of squares or min reconstruction error
                
                # Initialise iterations and define max number of iterations
                iter     <- 1
                max_iter <- 1000
                # Initialise the two factorisation matrices with random values
                # (uniform distribution)
                P <- matrix(runif(r*n, min=0.01, max=1), nrow=r, ncol=n)
                M <- matrix(runif(m*r, min=0.01, max=1), nrow=m, ncol=r)
                
                # Iteration zero
                P   <- P*(t(M)%*%V)/(t(M)%*%M%*%P)
                M   <- M*(V%*%t(P))/(M%*%P%*%t(P))
                Vr  <- M%*%P          # Reconstructed matrix
                RSS <- sum((V-Vr)^2)
                SST <- sum((V-mean(V))^2)
                R2[iter] <- 1-(RSS/SST)
                
                # l2-norm normalisation which eliminates trivial scale indeterminacies
                for (kk in 1:r) {
                    norm    <- sqrt(sum(M[, kk]^2))
                    M[, kk] <- M[, kk]/norm
                    P[kk, ] <- P[kk, ]*norm
                }
                
                # Start iterations for NMF convergence
                for (iter in iter:max_iter)  {
                    P   <- P*(t(M)%*%V)/(t(M)%*%M%*%P)
                    M   <- M*(V%*%t(P))/(M%*%P%*%t(P))
                    Vr  <- M%*%P
                    RSS <- sum((V-Vr)^2)
                    SST <- sum((V-mean(V))^2)
                    R2[iter] <- 1-(RSS/SST)
                    
                    # l2-norm normalisation
                    for (kk in 1:r) {
                        norm    <- sqrt(sum(M[, kk]^2))
                        M[, kk] <- M[, kk]/norm
                        P[kk, ] <- P[kk, ]*norm
                    }
                    
                    # Check if the increase of R2 in the last 20 iterations is less than the target
                    if (iter>20) {
                        R2_diff <- R2[iter]-R2[iter-20]
                        if (R2_diff<R2[iter]*R2_target/100) {
                            break
                        }
                    }
                }
                R2_choice[j] <- R2[iter]
                
                M_temp[[j]]  <- M
                P_temp[[j]]  <- P
            }
            
            choice <- which.max(R2_choice)
            
            R2_cross[r] <- R2_choice[choice]
            M_list[[r]] <- M_temp[[choice]]
            P_list[[r]] <- P_temp[[choice]]
        }
        
        # Choose the minimum number of synergies using the R2 criterion
        MSE  <- 100                     # Initialise the Mean Squared Error (MSE)
        iter <- 0                       # Initialise iterations
        while (MSE>1e-04) {
            iter <- iter+1
            if (iter==r-1) {
                break
            }
            R2_interp <- as.data.frame(cbind(c(1:(r-iter+1)), R2_cross[iter:r]))
            colnames(R2_interp) <- c("synergies", "R2_values")
            n <- nrow(R2_interp)
            linear <- lm(R2_values ~ synergies, R2_interp)
            linear_points <- linear[[5]]
            MSE <- sum((linear_points-R2_interp[, 2])^2)/n
        }
        syns_R2 <- iter
        
        return(list(M=M_list[[syns_R2]],
                    P=P_list[[syns_R2]]))
    }
    
    # Find different conditions
    # (trials must be named as stated in the beginning of this script)
    # "*": 0 or more
    # "+": 1 or more
    conditions <- gsub("SYNS_P[0-9]+_", "", rownames(data))
    conditions <- unique(gsub("_Syn.*", "", conditions))
    conditions <- unique(gsub("_[0-9]+$", "", conditions))
    
    # Build list with the trials of the different conditions
    all_trials <- vector("list", length(conditions))
    names(all_trials) <- conditions
    for (condition in conditions) {
        all_trials[[condition]] <- data[grep(paste0("_", condition, "_"), rownames(data)), ]
    }
    
    # Apply NMF
    sys_date <- gsub(" [0-9][0-9]:[0-9][0-9]:[0-9][0-9]$", "", Sys.time())
    sys_time <- gsub("^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] ", "", Sys.time())
    
    message("\nClassify motor primitives using NMF\nStarted on ", sys_date, " at ", sys_time)
    
    tictoc <- system.time({
        data_NMF <- parallel::parLapply(cl, all_trials, NMFn)
    })
    
    ll <- sum(unlist(lapply(data_NMF, function(x) nrow(x$M))))
    
    message("\n- - - - - - - - - - - - - - - - - - - - - - -\n",
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
    
    # Define centre of activity (CoA)
    CoA <- function(x) {
        
        points <- length(x)
        
        AA <- numeric()
        BB <- numeric()
        
        for (pp in 1:points) {
            alpha  <- 360*(pp-1)/(points-1)*pi/180
            vec    <- x[pp]
            AA[pp] <- vec*cos(alpha)
            BB[pp] <- vec*sin(alpha)
        }
        AA <- sum(AA)
        BB <- sum(BB)
        
        CoAt <- atan(BB/AA)*180/pi
        
        # To keep the sign
        if (AA>0 && BB>0) {
            CoAt <- CoAt  
        } else if (AA<0 && BB>0) {
            CoAt <- CoAt+180
        }        else if (AA<0 && BB<0) {
            CoAt <- CoAt+180
        }        else if (AA>0 && BB<0) {
            CoAt <- CoAt+360
        }
        
        CoAt*points/360
    }
    
    data_all   <- data
    order_list <- list()
    
    for (ll in seq_along(data_NMF)) {
        temp <- data_NMF[[ll]]
        cond <- names(data_NMF[ll])
        
        data_NMF_P <- temp$P
        data_NMF_M <- temp$M
        syns_num_n <- ncol(data_NMF_M)
        
        # Order using CoA
        orders <- order(apply(data_NMF_P, 1, CoA))
        
        # # Another option would be to order using the global maximum
        # (just uncomment the next line if you want to try)
        # orders <- order(apply(data_NMF_P, 1, function(x) which.max(x)[1]))
        
        data_NMF_P <- data_NMF_P[orders, ]
        data_NMF_M <- data_NMF_M[, orders]
        
        # Normalise to 1
        data_NMF_P <- data.frame(t(apply(data_NMF_P, 1, function(x) x/max(x))))
        data_NMF_M <- data.frame(apply(data_NMF_M, 2, function(x) x/max(x)))
        
        rownames(data_NMF_P) <- paste0("Syn", 1:nrow(data_NMF_P))
        colnames(data_NMF_M) <- paste0("Syn", 1:ncol(data_NMF_M))
        
        # Plot classified syns
        # Find plot size
        dev_size <- dev.size(units="in")
        # Margins are specified in inches following the order:
        # bottom, left, top, right
        # Reduction factor of margins to account for screens at different resolutions
        red_factor <- 35
        par(mfrow=c(syns_num_n, 2),
            mai=c(dev_size[2]/red_factor,
                  dev_size[1]/red_factor,
                  dev_size[2]/red_factor,
                  dev_size[1]/red_factor))
        
        for (syn in 1:syns_num_n) {
            plot(x=c(1:ncol(data_NMF_P)), y=data_NMF_P[syn, ],
                 ty="l", main=paste0("Synergy ", syn, ", ", cond),
                 xlab="", ylab="",
                 xaxt="n", yaxt="n", lwd=2)
            barplot(sort(data_NMF_M[, syn], decreasing=T))
            abline(h=seq(0.2, 0.8, 0.1), col=2)
            tot <- length(data_NMF_M[, syn])
            abline(v=tot*seq(0, 1, 0.25), col=2)
        }
        
        qq <- 1
        while (!is.na(qq)) {
            message("\nDo you want to change order of classified synergies (type 'y' for 'yes' or 'n' for 'no')?")
            qq <- readline()
            # Break if user decides
            if (qq=="y" || qq=="n") break
        }
        
        if (qq=="y" || qq=="ye" || qq=="yes") {
            # Prompt for decision
            message("Press Esc to stop (order will not be changed)")
            orders_new <- numeric()
            for (cc in 1:nrow(data_NMF_P)) {
                pp <- 0.2
                while (pp<1) {
                    pp <- readline(paste0("Syn num to be associated with the curve ", cc, ": "))
                    
                    if (grepl("^$", pp)) {
                        pp <- -1
                    } else if (grepl("\\D", pp) && !grepl("^s$", pp)) {
                        pp <- -1
                    } else if (grepl("^s$", pp)) {
                        pp <- 1000
                    } else if  (as.numeric(pp)>syns_num_n){
                        pp <- -1
                    }
                }
                orders_new[cc] <- pp
            }
            
            orders_new <- as.numeric(orders_new)
            
            orders <- sort.int(orders_new, index.return=T)$ix
            
            data_NMF_P <- data_NMF_P[orders, ]
            data_NMF_M <- data_NMF_M[, orders]
            
            # Normalise to 1
            data_NMF_P <- data.frame(t(apply(data_NMF_P, 1, function(x) x/max(x))))
            data_NMF_M <- data.frame(apply(data_NMF_M, 2, function(x) x/max(x)))
            
            rownames(data_NMF_P) <- paste0("Syn", 1:nrow(data_NMF_P))
            colnames(data_NMF_M) <- paste0("Syn", 1:ncol(data_NMF_M))
            
            # Re-plot classified syns
            par(mfrow=c(syns_num_n, 2),
                mai=c(dev_size[2]/red_factor,
                      dev_size[1]/red_factor,
                      dev_size[2]/red_factor,
                      dev_size[1]/red_factor))
            
            for (syn in 1:syns_num_n) {
                plot(x=c(1:ncol(data_NMF_P)), y=data_NMF_P[syn, ],
                     ty="l", main=paste0("Synergy ", syn, ", ", cond),
                     xlab="", ylab="",
                     xaxt="n", yaxt="n", lwd=2)
                barplot(sort(data_NMF_M[, syn], decreasing=T))
                abline(h=seq(0.2, 0.8, 0.1), col=2)
                tot <- length(data_NMF_M[, syn])
                abline(v=tot*seq(0, 1, 0.25), col=2)
            }
            Sys.sleep(2)
        }
        
        # Now search for syns having module bigger than "M_threshold"
        # If a syn has more than one module, choose the one with the highest value
        # Then compare the primitive to the relevant one and assess similarity
        # If similarity is lower than "R2_threshold", classify as combined, otherwise keep
        temp <- data_NMF_M
        
        # Determine the threshold for M and R2
        # Calculate the M threshold
        M_threshold <- mean(colMeans(temp, na.rm=T), na.rm=T)
        
        temp[temp<M_threshold] <- NA
        
        quality <- numeric()
        
        for (tt in 1:nrow(temp)) {
            if (sum(temp[tt, ], na.rm=T)==0) {
                quality[tt] <- NA
                next
            } else if (sum(is.na(temp[tt, ]))>=1 || sum(temp[tt, ], na.rm=T)!=0) {
                # Find position of maximum
                choice <- which(temp[tt, ]==max(temp[tt, ], na.rm=T))
                # Discard the others
                temp[tt, -choice] <- NA
                # Calculate R2 between curve and primitive
                P1  <- as.numeric(data_NMF_P[choice, ])
                P2  <- as.numeric(data[tt, ])
                RSS <- sum((P1-P2)^2)
                SST <- sum((P1-mean(P1))^2)
                R2  <- 1-(RSS/SST)
                
                quality[tt] <- R2
            }
        }
        
        # Calculate the R2 threshold
        R2_threshold <- mean(quality, na.rm=T)
        if (sign(R2_threshold)==1) {
            R2_threshold <- R2_threshold/4
        } else if (sign(R2_threshold)==-1) {
            R2_threshold <- R2_threshold*4
        }
        
        quality[quality<=R2_threshold] <- NA
        
        classification <- numeric()
        weight <- numeric()
        
        for (tt in 1:nrow(temp)) {
            if (sum(temp[tt, ], na.rm=T)==0) {
                classification[tt] <- NA
                weight[tt] <- NA
                next
            } else if (sum(is.na(temp[tt, ]))>=1 || sum(temp[tt, ], na.rm=T)!=0) {
                # Find position of maximum
                choice <- which(temp[tt, ]==max(temp[tt, ], na.rm=T))
                # Discard the others
                temp[tt, -choice] <- NA
                # Discard if R2 between curve and primitive is lower than R2_threshold
                R2 <- quality[tt]
                
                if (is.na(R2)) {
                    classification[tt] <- NA
                    weight[tt] <- NA
                } else {
                    classification[tt] <- choice
                    weight[tt] <- temp[tt, choice]
                }
            }
        }
        
        trial <- gsub("_Syn.*", "", rownames(temp))
        syn   <- as.numeric(gsub(".*_Syn", "", rownames(temp)))
        
        ordered <- data.frame(trial, syn, classification, weight, quality)
        colnames(ordered) <- c("trial", "syn_original", "syn_classified", "weight", "quality")
        
        syns_num <- max(ordered$syn_classified, na.rm=T)
        
        ordered$syn_classified[which(is.na(ordered$syn_classified))] <- "combined"
        
        # Remove double classifications, if present
        # Find unique trial names
        trials <- which(!duplicated(ordered$trial))
        for (uu in seq_along(trials)) {
            
            # Read the classification
            if (uu<length(trials)) {
                temp1 <- ordered[trials[uu]:(trials[uu+1]-1), ]
            } else {
                temp1 <- ordered[trials[uu]:nrow(ordered), ]
            }
            
            # Find duplicates
            temp2 <- which(duplicated(temp1$syn_classified))
            
            if (length(temp2)==0) {
                next
            } else {
                for (syn in c((1:syns_num), "combined")) {
                    
                    temp2 <- grep(paste0("^", syn, "$"), temp1$syn_classified)
                    
                    if (length(temp2)<=1) {
                        next
                    } else {
                        # Remove lower quality trials
                        temp3 <- as.numeric(temp1$quality[temp2])
                        temp2 <- temp2[-which(temp3==max(temp3))]
                        temp1$syn_classified[temp2] <- NA
                    }
                }
            }
            # Save the new data
            if (uu<length(trials)) {
                ordered[trials[uu]:(trials[uu+1]-1), ] <- temp1
            } else {
                ordered[trials[uu]:nrow(ordered), ] <- temp1
            }
        }
        
        ordered$syn_classified[which(is.na(ordered$syn_classified))] <- "combined"
        
        ordered <- data.frame(old=paste0(ordered$trial, "_Syn", ordered$syn_original),
                              new=paste0(ordered$trial, "_Syn", ordered$syn_classified),
                              ordered)
        
        ordered[] <- lapply(ordered, as.character)
        
        combined <- length(grep("combined", ordered$syn_classified))
        total    <- nrow(ordered)
        
        message("\n  Locomotion type: ", cond,
                "\n  Total synergies: ", total,
                "\n       Recognised: ", total-combined,
                "\n         Combined: ", combined, " (~", round(combined/total*100, 0), "%)",
                "\n     R2 threshold: ", round(R2_threshold, 2),
                "\n Module threshold: ", round(M_threshold, 2), "\n")
        
        syn_perc <- numeric()
        for (ss in 1:syns_num) {
            syn_perc[ss] <- round(length(grep(paste0("^", ss, "$"),
                                              ordered$syn_classified))/length(trials)*100, 0)
            message("             Syn", ss, ": ", syn_perc[ss], "%")
        }
        
        order_list[[ll]] <- ordered
    }
    
    orders <- plyr::ldply(order_list, data.frame)
    
    # Rename synergies in the correct order and save
    SYNS_classified <- SYNS
    
    # Find unique trial names
    trials <- which(!duplicated(orders$trial))
    for (uu in seq_along(trials)) {
        
        # Read the classification
        if (uu<length(trials)) {
            classification <- orders$syn_classified[trials[uu]:(trials[uu+1]-1)]
        } else {
            classification <- orders$syn_classified[trials[uu]:nrow(orders)]
        }
        
        trial <- orders$trial[trials[uu]]
        
        colnames(SYNS_classified[[trial]]$P) <- c("time", paste0("Syn", classification))
        colnames(SYNS_classified[[trial]]$M) <- paste0("Syn", classification)
    }
    
    message("\nSaving classified synergies...")
    save(SYNS_classified, file=paste0(data_path, "SYNS_classified.RData"))
    message("...done!")
}

# STEP 4 - Plot muscle synergies ----
message("\n##################################",
        "\n STEP 4/4 - Plot muscle synergies",
        "\n##################################")

if (all(!grepl("^SYNS_classified$", objects()))) {
    load(paste0(data_path, "SYNS_classified.RData"))
}

# Create "Graphs/NMF" folder if it does not exist
path_for_graphs <- paste0(graph_path, "NMF", .Platform$file.sep)
dir.create(path_for_graphs, showWarnings=F)

# Define graphs export parameters and aesthetics
ty       <- "png"   # File type
re       <- 280     # Resolution in dpi
wi       <- 2000    # Width in pixels
he       <- 2500    # Height in pixels
mte      <- 36      # Main title text size
s_line   <- 0.9     # Line size
s_min    <- 0.05
c_back   <- "white" # Background colour
c_bord   <- "gray"  # Background border colour
c_min    <- "gray"  # Minor gridlines colour
c_bars   <- "black"
c_signal <- "black"
c_thin   <- "grey70"

# Get concatenated primitives
SYNS_P <- lapply(SYNS_classified, function(x) x$P)
SYNS_M <- lapply(SYNS_classified, function(x) x$M)

# Calculate mean motor primitives and remove combined
SYNS_P_all <- lapply(SYNS_P, function(x) {
    points <- max(x$time)
    x$time <- NULL
    x[, grep("combined", colnames(x))] <- NULL
    
    if (ncol(x)>0) {
        temp   <- matrix(0, nrow=points, ncol=ncol(x))
        
        colnames(temp) <- colnames(x)
        
        for (cc in seq(1, (1+nrow(x)-points), points)) {
            temp <- temp+x[c(cc:(cc+points-1)), ]
        }
        
        # Divide by the number of cycles to get mean value
        temp <- temp/(nrow(x)/points)
        
        # Minimum subtraction
        x <- apply(temp, 2, function(y) y-min(y))
        # Amplitude normalisation
        x <- apply(temp, 2, function(y) y/max(y))
    }
    
    return(data.frame(x))
})

# Remove combined motor modules
SYNS_M_all <- lapply(SYNS_M, function(x) {
    x <- data.frame(x)
    x[, grep("combined", colnames(x))] <- NULL
    return(data.frame(x))
})

max_syns   <- max(unlist(lapply(SYNS_M_all, function(x) ncol(x))))
conditions <- gsub("^SYNS_P[0-9]+_", "", names(SYNS_M_all))
conditions <- unique(gsub("_[0-9]+$", "", conditions))

# Progress bar
pb <- progress::progress_bar$new(format="[:bar]:percent ETA: :eta",
                                 total=length(conditions), clear=F, width=50)

message("\nExporting graphs...\n")

for (condition in conditions) {
    
    pb$tick()
    
    Cairo(file=paste0(path_for_graphs, "SYNS_", condition, ".", ty),
          type=ty, width=wi, height=he, pointsize=mte, dpi=re)
    
    SYNS_P_temp <- SYNS_P_all[grep(paste0("_", condition, "_"), names(SYNS_P_all))]
    SYNS_M_temp <- SYNS_M_all[grep(paste0("_", condition, "_"), names(SYNS_M_all))]
    
    varlist <- list()
    
    for (syn in 1:max_syns) {
        
        # Select relevant synergies
        data_P <- lapply(SYNS_P_temp, function(x) t(x[, grep(paste0("^Syn", syn), colnames(x))]))
        data_M <- lapply(SYNS_M_temp, function(x) {
            muscles <- rownames(x)
            x <- x[, grep(paste0("^Syn", syn), colnames(x))]
            if (length(x)>0) names(x) <- muscles
            t(x)
        })
        
        # Put primitives in a single data frame
        data_P <- plyr::ldply(data_P, data.frame, .id="trial")
        # Put modules in a single data frame
        data_M <- plyr::ldply(data_M, data.frame, .id="trial")
        
        data_P_av <- data.frame(time=c(1:(ncol(data_P)-1)),
                                value=colMeans(data_P[, -1]))
        data_P_sd <- data.frame(time=c(1:(ncol(data_P)-1)),
                                ymin=data_P_av$value-apply(data_P[, -1], 2, sd),
                                ymax=data_P_av$value+apply(data_P[, -1], 2, sd))
        
        data_M_av <- data.frame(value=colMeans(data_M[, -1]))
        data_M_av <- data.frame(muscle=rownames(data_M_av),
                                data_M_av)
        
        data_P <- reshape2::melt(data_P, id="trial")
        data_M <- reshape2::melt(data_M, id="trial")
        
        temp_P <- ggplot2::ggplot() +
            ggplot2::ggtitle(paste0("Motor primitive ", syn)) +
            ggplot2::ylim(-0.2, 1.2) +
            ggplot2::geom_ribbon(data=data_P_sd,
                                 ggplot2::aes(x=time, ymin=ymin, ymax=ymax),
                                 fill="grey80") +
            ggplot2::geom_line(data=data_P_av,
                               ggplot2::aes(x=time, y=value),
                               colour=c_signal, size=s_line) +
            ggplot2::theme(axis.title=ggplot2::element_blank(),
                           panel.background=ggplot2::element_rect(fill=c_back, colour=c_bord),
                           panel.grid.major=ggplot2::element_line(colour=c_min, size=s_min),
                           panel.grid.minor=ggplot2::element_blank(),
                           legend.position="none")
        
        temp_M <- ggplot2::ggplot() +
            ggplot2::ggtitle(paste0("Motor module ", syn)) +
            ggplot2::ylim(0, 1) +
            ggplot2::geom_hline(yintercept=c(0.25, 0.5, 0.75, 1), size=s_min, color=c_thin) +
            ggplot2::geom_bar(data=data_M_av,
                              ggplot2::aes(x=muscle, y=value),
                              fill=c_bars, alpha=0.3,
                              stat="identity") +
            ggplot2::scale_x_discrete(limits=data_M_av$muscle) +
            ggplot2::geom_jitter(data=data_M,
                                 ggplot2::aes(x=variable, y=value),
                                 fill=c_bars, width=0.1, size=0.1) +
            ggplot2::theme(axis.title=ggplot2::element_blank(),
                           axis.text.y=ggplot2::element_blank(),
                           panel.background=ggplot2::element_rect(fill=c_back, colour=c_bord),
                           panel.grid.major=ggplot2::element_blank(),
                           panel.grid.minor=ggplot2::element_blank(),
                           axis.ticks=ggplot2::element_blank(),
                           legend.position="none")
        
        varname_P <- paste0("r", 2*syn)
        varname_M <- paste0("r", 2*syn-1)
        varlist[[2*syn]]   <- assign(varname_P, temp_P)
        varlist[[2*syn-1]] <- assign(varname_M, temp_M)
    }
    
    suppressWarnings(gridExtra::grid.arrange(grobs=varlist, nrow=max_syns, ncol=2,
                                             top=(paste0("Synergies - ", condition))))
    
    dev.off() # Close Cairo export
}
message("\n...done!")