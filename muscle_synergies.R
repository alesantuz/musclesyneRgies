# The minimum requirements for successfully running this script are:
# 1. Raw EMG data prepared in RData format, one list where each named element is a trial
#    formatted as data frame with columns named "time" and muscle abbreviations,
#    file saved as "RAW_EMG.RData"
# 2. Gait cycle times prepared in RData format, one list where each named element is a trial
#    formatted as data frame with columns named "touchdown" and "stance", times in [s],
#    file saved as "CYCLE_TIMES.RData"
# 3. Trials are named as "CYCLE_TIMES_Pxxxx_AA_yy" or "RAW_EMG_Pxxxx_AA_yy", where:
#    - Pxxxx is the participant number (e.g. P0002 or P0456)
#    - AA is the condition (e.g. TW for treadmill walking or OR for overground running, etc.)
#    - yy is the trial number (e.g. 01 or 15)

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
               "plyr")

pkgs_new <- pkgs_list[!(pkgs_list %in% installed.packages()[,"Package"])]
if(length(pkgs_new)) install.packages(pkgs_new)

# Load required packages
lapply(pkgs_list, library, character.only=T)
# ATTENTION! The following line removes all objects from the global environment
# except for the ones listed ("cl" and "free_cores" in this case)
rm(list=setdiff(ls(), c("cl", "free_cores")))

# Where are your files located?
if (exists("choose.dir")) {
    data_path <- paste0(choose.dir(caption="Select data folder"), "\\")
} else {
    data_path <- paste0(tcltk::tk_choose.dir(caption="Select data folder"), "\\")
}

# Create "Graphs" folder if it does not exist
graph_path <- paste0(data_path, "Graphs\\")
dir.create(graph_path, showWarnings=F)

# Create cluster for parallel computing if not already done
clusters <- objects()

if (sum(grepl("^cl$", clusters))==0) {
    # Decide how many processor threads have to be excluded from the cluster
    # It is a good idea to leave at least one free, so that the machine can be used during computation
    free_cores <- 1
    cl <- makeCluster(detectCores()-free_cores)
}

# STEP 1 - Raw data processing ----
message("\n################################",
        "\n STEP 1/3 - Raw data processing",
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
    
    # Create "Graphs\\EMG" folder if it does not exist
    path_for_graphs <- paste0(graph_path, "EMG\\")
    dir.create(path_for_graphs, showWarnings=F)
    
    # Global parameters, change as needed
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
    
    pb <- progress_bar$new(format="[:bar]:percent ETA: :eta",
                           total=length(RAW_EMG), clear=F, width=50)
    
    message("\nApply filters")
    
    for (ii in seq_along(RAW_EMG)) {
        
        pb$tick()
        
        trial <- gsub("RAW_EMG_", "", names(RAW_EMG[ii]))
        muscles <- names(RAW_EMG[[ii]])
        
        # Cut to the first cy_max+2 cycles
        c_time <- CYCLE_TIMES[[grep(trial, names(CYCLE_TIMES))]]
        cut <- c_time$touchdown[cy_max+2]
        
        # Check if there are more than cy_max cycles and do not cut if false
        label <- which(RAW_EMG[[ii]]$time>cut)[1]
        
        if (is.na(label)) {
            emg_data <- RAW_EMG[[ii]]
        } else {
            emg_data <- RAW_EMG[[ii]][1:(label-1),]
        }
        
        # Demean EMG (subtract mean value from the signal to eliminate offset shifts)
        time     <- emg_data$time
        emg_data <- apply(emg_data, 2, function(x) x-mean(x, na.rm=T))
        emg_data[, "time"] <- time
        
        emg_data_temp <- emg_data
        
        # Filtering
        # EMG system acquisition frequency [Hz]
        freq <- round(1/(emg_data[, "time"][3]-emg_data[, "time"][2]), 0)
        
        # High-pass IIR (Infinite Impulse Response) Butterworth zero-phase filter design
        # Critical frequencies must be between 0 and 1, where 1 is the Nyquist frequency
        # "filtfilt" is for zero-phase filtering
        HPfn <- HPf/(freq/2)                          # Normalise by the Nyquist frequency (f/2)
        HP   <- butter(HPo, HPfn, type="high")
        for (kk in 2:ncol(emg_data_temp)) {
            emg_data_temp[,kk] <- filtfilt(HP, emg_data_temp[,kk])
        }
        
        # Full-wave rectification
        emg_data_temp <- abs(emg_data_temp)
        
        # Low-pass IIR (Infinite Impulse Response) Butterworth zero-phase filter design
        # Critical frequencies must be between 0 and 1, where 1 is the Nyquist frequency
        # "filtfilt" is for zero-phase filtering
        LPfn <- LPf/(freq/2)                            # Normalise by the Nyquist frequency (f/2)
        LP   <- butter(LPo, LPfn, type="low")
        for (kk in 2:ncol(emg_data_temp)) {
            emg_data_temp[,kk] <- filtfilt(LP, emg_data_temp[,kk])
        }
        
        emg_data_temp[emg_data_temp<0] <- 0             # Set negative values to zero
        temp <- emg_data_temp
        temp[temp==0] <- Inf
        emg_data_temp[emg_data_temp==0] <- min(temp)    # Set the zeros to the smallest nonzero entry
        
        emg_data_filt <- emg_data_temp[,-1]
        
        # Subtract the minimum
        emg_data_filt <- apply(emg_data_filt, 2, function(x) x-min(x))
        # Amplitude normalisation to the maximum of the trial
        emg_data_filt <- apply(emg_data_filt, 2, function(x) x/max(x))
        
        emg_time <- seq(emg_data[, "time"][1], emg_data[, "time"][nrow(emg_data_filt)], 1/freq)
        
        # Isolate cycles and normalise time to 200 points (first 100 stance, second 100 swing)
        cycs <- nrow(c_time)-1
        if (cycs>cy_max) cycs <- cy_max
        
        # Isolate each cycle
        for (jj in 1:cycs) {
            # Stance
            temp <- data.frame()
            t1   <- c_time$touchdown[jj]
            t2   <- c_time$touchdown[jj]+c_time$stance[jj]
            
            if (t1>max(emg_time, na.rm=T) || t2>max(emg_time, na.rm=T)) {
                cycs <- jj-1
                break
            } else {
                t1 <- which(emg_time>=t1)[1]
                t2 <- which(emg_time>=t2)[1]
                temp  <- emg_data_filt[t1:t2,]
            }
            
            # Check if there is data
            if (sum(temp, na.rm=T)==0) next
            
            temp1 <- numeric()
            temp2 <- data.frame(matrix(1:points, nrow=points/2, ncol=1))
            
            # Interpolate each channel to (points/2) points
            for (kk in 1:ncol(temp)) {
                temp1 <- as.data.frame(approx(temp[,kk], method="linear", n=points/2))
                temp1 <- temp1[,2]
                temp2 <- cbind(temp2, temp1)
            }
            colnames(temp2) <- muscles
            
            # Swing
            temp <- data.frame()
            t1   <- c_time$touchdown[jj]+c_time$stance[jj]
            t2   <- c_time$touchdown[jj+1]
            
            if (t1>max(emg_time, na.rm=T) || t2>max(emg_time, na.rm=T)) {
                cycs <- jj-1
                break
            } else {
                t1 <- which(emg_time>=t1)[1]
                t2 <- which(emg_time>=t2)[1]
                temp  <- emg_data_filt[t1:t2,]
            }
            
            # Check if there is data
            if (sum(temp, na.rm=T)==0) next
            temp1 <- numeric()
            temp3 <- data.frame(matrix(1:points, nrow=points/2, ncol=1))
            # Interpolate each channel to (points/2) points
            for (kk in 1:ncol(temp)) {
                temp1 <- as.data.frame(approx(temp[,kk], method="linear", n=points/2))
                temp1 <- temp1[,2]
                temp3 <- cbind(temp3, temp1)
            }
            colnames(temp3) <- muscles
            
            temp4 <- rbind(temp2, temp3)
            
            # Set every value >1 to 1
            temp4[temp4>1] <- 1
            temp4$time     <- c(1:points)
            
            # For the concatenated data
            if (jj==1) {
                emg_data_co <- temp4
                
                # For the averaged data
                emg_data_av <- matrix(0, nrow=points, ncol=ncol(emg_data_co))
            } else {
                emg_data_co <- rbind(emg_data_co, temp4)
                
                # For the averaged data
                emg_data_av <- emg_data_av+temp4
            }
        }
        
        FILT_EMG[[ii]] <- emg_data_co
        names(FILT_EMG[[ii]]) <- muscles
        list_names[ii] <- paste0("FILT_EMG_", trial)
        
        # Export average cycles for checking
        # Normalise the averaged data
        emg_data_av <- data.frame(apply(emg_data_av, 2, function(x) x/max(x)))
        emg_data_av[, "time"] <- c(1:points)
        
        # Make graphs
        Cairo(file=paste0(path_for_graphs, "temp.png", sep=""),
              width=1500, height=1500, pointsize=12, dpi=120)
        varlist <- list()
        
        for (mm in 2:length(muscles)) {
            data <- data.frame(emg_data_av$time,
                               emg_data_av[, grep(paste0("^", muscles[mm], "$"), colnames(emg_data_av))])
            colnames(data) <- c("time", "signal")
            
            varname <- paste("pp", mm, sep = "")
            
            temp <- ggplot() + ggtitle(muscles[mm]) +
                geom_line(data=data, aes(x=time,  y=signal), colour="black", size=0.9) +
                theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
                theme(panel.background=element_rect(fill="white", colour="gray")) +
                theme(panel.grid.minor=element_line(colour="gray", size=0.05)) +
                ylim(0, 1) +
                theme(legend.position = "none")
            varlist[[mm-1]] <- assign(varname, temp)
        }
        
        grid.arrange(grobs=varlist,
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
        "\n STEP 2/3 - Synergies extraction",
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
    synsNMFn <- function (V, ...)
    {
        R2_target <- 0.01                           # Convergence criterion (percent of the R2 value)
        R2_cross  <- numeric()                      # R2 values for cross validation and syns number asessment
        W_list    <- list()                         # To save factorisation W matrices (synergies)
        H_list    <- list()                         # To save factorisation H matrices (primitives)
        Vr_list   <- list()                         # To save factorisation Vr matrices (reconstructed signals)
        iters     <- numeric()                      # To save the iterations number
        
        # Original matrix
        time   <- V$time
        V$time <- NULL
        V      <- as.matrix(t(V))                   # Needs to be transposed for NMF
        V[V<0] <- 0                                 # Set negative values to zero
        temp   <- V
        temp[temp==0] <- Inf
        V[V==0] <- min(temp, na.rm=T)               # Set the zeros to the smallest nonzero entry in V
        
        m <- nrow(V)                                # Number of muscles
        n <- ncol(V)                                # Number of time points
        
        max_syns <- m-round(m/4, 0)                 # Max number of syns is m-(m/4)
        
        for (r in 1:max_syns) {                     # Run NMF with different initial conditions (syns num.)
            R2_choice <- numeric()                  # Collect the R2 values for each syn and choose the max
            
            # Preallocate to then choose those with highest R2
            W_temp  <- list()
            H_temp  <- list()
            Vr_temp <- list()
            
            for (j in 1:5) {                        # Run NMF 5 times for each syn and choose best run
                # To save error values
                R2  <- numeric()                    # 1st cost function (R squared)
                SST <- numeric()                    # Total sum of squares
                RSS <- numeric()                    # Residual sum of squares or min. reconstr. error
                
                # Initialise iterations and define max number of iterations
                iter     <- 1
                max_iter <- 1000
                # Initialise the two factorisation matrices with random values (uniform distribution)
                H <- matrix(runif(r*n, min=0.01, max=1), nrow=r, ncol=n)
                W <- matrix(runif(m*r, min=0.01, max=1), nrow=m, ncol=r)
                
                # Iteration zero
                H   <- H * (t(W) %*% V) / (t(W) %*% W %*% H)
                W   <- W * (V %*% t(H)) / (W %*% H %*% t(H))
                Vr  <- W %*% H                       # Reconstructed matrix
                RSS <- sum((V-Vr)^2)
                SST <- sum((V-mean(V))^2)
                R2[iter] <- 1-(RSS/SST)
                
                # l2-norm normalisation which eliminates trivial scale indeterminacies
                # The cost function doesn't change. Impose ||W||2 = 1 and normalise H accordingly.
                # ||W||2, also called L2,1 norm or l2-norm, is a sum of Euclidean norm of columns.
                for (kk in 1:r) {
                    norm   <- sqrt(sum(W[,kk]^2))
                    W[,kk] <- W[,kk]/norm
                    H[kk,] <- H[kk,]*norm
                }
                
                # Start iterations for NMF convergence
                for (iter in iter:max_iter)  {
                    H   <- H * (t(W) %*% V) / (t(W) %*% W %*% H)
                    W   <- W * (V %*% t(H)) / (W %*% H %*% t(H))
                    Vr  <- W %*% H
                    RSS <- sum((V-Vr)^2)
                    SST <- sum((V-mean(V))^2)
                    R2[iter] <- 1-(RSS/SST)
                    
                    # l2-norm normalisation
                    for (kk in 1:r) {
                        norm   <- sqrt(sum(W[,kk]^2))
                        W[,kk] <- W[,kk]/norm
                        H[kk,] <- H[kk,]*norm
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
                
                W_temp[[j]]  <- W
                H_temp[[j]]  <- H
                Vr_temp[[j]] <- Vr
            }
            
            choice <- which.max(R2_choice)
            
            R2_cross[r]  <- R2_choice[choice]
            W_list[[r]]  <- W_temp[[choice]]
            H_list[[r]]  <- H_temp[[choice]]
            Vr_list[[r]] <- Vr_temp[[choice]]
            iters[r]     <- iter
        }
        
        # Choose the minimum number of synergies using the R2 criterion
        MSE  <- 100                                 # Initialise the Mean Squared Error (MSE)
        iter <- 0                                   # Initialise iterations
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
            MSE     <- sum((lin_pts-R2_interp[,2])^2)/nn
        }
        syns_R2 <- iter
        
        H_choice <- data.frame(time, t(H_list[[syns_R2]]))
        colnames(H_choice) <- c("time", paste0("Syn", 1:(ncol(H_choice)-1)))
        rownames(H_choice) <- NULL
        
        return(list(synsR2=as.numeric(syns_R2),
                    W=W_list[[syns_R2]],
                    H=H_choice,
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
        SYNS <- parLapply(cl, FILT_EMG, synsNMFn)
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
            parallel::detectCores()-free_cores, " used",
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
        "\n STEP 3/3 - Classification of muscle synergies",
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
    
    # Create "Graphs\\NMF" folder if it does not exist
    path_for_graphs <- paste0(graph_path, "NMF\\")
    dir.create(path_for_graphs, showWarnings=F)
    
    # Get concatenated primitives
    SYNS_H <- lapply(SYNS, function(x) x$H)
    
    # Make sure that all motor primitives are normalized to the same amount of points
    points <- unlist(lapply(SYNS_H, function(x) max(x$time)))
    
    if (sd(points)!=0) {
        message("\nNot all motor primitives are normalised to the same amount of points!",
                "\nPlease re-check your data\n")
    } else points <- unique(points)
    
    message("\nCalculating mean gait cycles...")
    
    # Progress bar
    pb <- progress_bar$new(format="[:bar]:percent ETA: :eta",
                           total=length(SYNS_H), clear=F, width=50)
    
    SYNS_H <- lapply(SYNS_H, function(x) {
        
        pb$tick()
        
        x$time <- NULL
        temp <- matrix(0, nrow=points, ncol=ncol(x))
        
        for (cc in seq(1, (1+nrow(x)-points), points)) {
            temp <- temp+x[c(cc:(cc+points-1)), ]
        }
        
        # Divide by the number of cycles to get mean value
        temp <- temp/(nrow(x)/points)
        
        # Amplitude normalisation
        x <- apply(temp, 2, function(y) y/(max(y)))
        
        # Transpose to facilitate visualization
        return(t(x))
    })
    message("...done!")
    
    message("\nPutting primitives in a single data frame...")
    
    # Progress bar
    pb <- progress_bar$new(format="[:bar]:percent ETA: :eta",
                           total=length(SYNS_H), clear=F, width=50)
    
    data <- plyr::ldply(SYNS_H, function(x) {
        pb$tick()
        data.frame(x)
    })
    message("...done!")
    
    # Give names to primitives (start from synergy zero because "make.unique" function works like that)
    # Find non-duplicated names and assign "Syn0" to them
    syn0 <- which(!duplicated(data$.id))
    data$.id <- make.unique(data$.id)
    data$.id[syn0] <- paste0(data$.id[syn0], "_Syn0")
    # Assign incremental Syn number to other names
    data$.id <- gsub("\\.", "_Syn", data$.id)
    # Start from Syn1 instead that from Syn0
    temp1 <- gsub("[0-9]$", "", data$.id)
    temp2 <- as.numeric(gsub(".*_Syn", "", data$.id))+1
    temp3 <- paste0(temp1, temp2)
    # Assign new names to row names and remove id column
    rownames(data) <- temp3
    data$.id <- NULL
    
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
    NMFn <- function(V, ...)
    {
        R2_target  <- 0.01                      # Convergence criterion (percent of the R2 value)
        R2_cross   <- numeric()                 # R2 values for cross validation and syns number asessment
        W_list     <- list()                    # To save factorisation W matrices (synergies)
        H_list     <- list()                    # To save factorisation H matrices (primitives)
        
        # Original matrix
        V <- as.matrix(V)
        V[V<0] <- 0                             # Set negative values to zero
        temp <- V
        temp[temp==0] <- Inf
        V[V==0] <- min(temp, na.rm=T)           # Set the zeros to the smallest nonzero entry in V
        
        m <- nrow(V)                            # Number of primitives in the data-set
        n <- ncol(V)                            # Number of time points
        
        # Determine the maximum number of synergies by searching for the maximum rank
        temp <- as.numeric(gsub(".*\\_Syn", "", rownames(V)))
        # Add one because interpolation must happen with at elast two points
        max_syns <- max(temp)+1
        
        for (r in 1:max_syns) {                 # Run NMF with different initial conditions (syns num.)
            R2_choice  <- numeric()             # Collect the R2 values for each syn and choose the max
            
            # Preallocate to then choose those with highest R2
            W_temp  <- list()
            H_temp  <- list()
            
            for (j in 1:5) {                       # Run NMF 10 times for each syn and choose best run
                # To save error values
                R2  <- numeric()                    # 1st cost function (R squared)
                SST <- numeric()                    # Total sum of squares
                RSS <- numeric()                    # Residual sum of squares or min. reconstr. error
                
                # Initialise iterations and define max number of iterations
                iter     <- 1
                max_iter <- 1000
                # Initialise the two factorisation matrices with random values (uniform distribution)
                H <- matrix(runif(r*n, min=0.01, max=1), nrow=r, ncol=n)
                W <- matrix(runif(m*r, min=0.01, max=1), nrow=m, ncol=r)
                
                # Iteration zero
                H   <- H * (t(W) %*% V) / (t(W) %*% W %*% H)
                W   <- W * (V %*% t(H)) / (W %*% H %*% t(H))
                Vr  <- W %*% H                       # Reconstructed matrix
                RSS <- sum((V-Vr)^2)
                SST <- sum((V-mean(V))^2)
                R2[iter] <- 1-(RSS/SST)
                
                # l2-norm normalisation which eliminates trivial scale indeterminacies
                # The cost function doesn't change. Impose ||W||2 = 1 and normalise H accordingly.
                # ||W||2, also called L2,1 norm or l2-norm, is a sum of Euclidean norm of columns.
                for (kk in 1:r) {
                    norm   <- sqrt(sum(W[,kk]^2))
                    W[,kk] <- W[,kk]/norm
                    H[kk,] <- H[kk,]*norm
                }
                
                # Start iterations for NMF convergence
                for (iter in iter:max_iter)  {
                    H   <- H * (t(W) %*% V) / (t(W) %*% W %*% H)
                    W   <- W * (V %*% t(H)) / (W %*% H %*% t(H))
                    Vr  <- W %*% H
                    RSS <- sum((V-Vr)^2)
                    SST <- sum((V-mean(V))^2)
                    R2[iter] <- 1-(RSS/SST)
                    
                    # l2-norm normalisation
                    for (kk in 1:r) {
                        norm   <- sqrt(sum(W[,kk]^2))
                        W[,kk] <- W[,kk]/norm
                        H[kk,] <- H[kk,]*norm
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
                
                W_temp[[j]]  <- W
                H_temp[[j]]  <- H
            }
            
            choice <- which.max(R2_choice)
            
            R2_cross[r] <- R2_choice[choice]
            W_list[[r]] <- W_temp[[choice]]
            H_list[[r]] <- H_temp[[choice]]
        }
        
        # Choose the minimum number of synergies using the R2 criterion
        MSE  <- 100                             # Initialise the Mean Squared Error (MSE)
        iter <- 0                               # Initialise iterations
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
            MSE <- sum((linear_points-R2_interp[,2])^2)/n
        }
        syns_R2 <- iter
        
        return(list(W=W_list[[syns_R2]],
                    H=H_list[[syns_R2]]))
    }
    
    # Find different conditions
    # (trials must be named as stated in the beginning of this script)
    # "*": 0 or more
    # "+": 1 or more
    conditions <- gsub("SYNS_P[0-9]+_", "", rownames(data))
    conditions <- unique(gsub("_.*", "", conditions))
    
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
        data_NMF <- parLapply(cl, all_trials, NMFn)
    })
    
    ll <- sum(unlist(lapply(data_NMF, function(x) nrow(x$W))))
    
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
            parallel::detectCores()-free_cores, " used",
            "\n\n        Number of trials: ", ll,
            "\n        Computation time: ", round(tictoc[[3]], 0), " s",
            "\nAverage trial comp. time: ", round(tictoc[[3]]/ll, 2), " s\n",
            "\n- - - - - - - - - - - - - - - - - - - - - - -")
    
    # Define center of activity (CoA)
    CoA <- function(x, ...) {
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
        if (AA>0 && BB>0) CoAt <- CoAt
        if (AA<0 && BB>0) CoAt <- CoAt+180
        if (AA<0 && BB<0) CoAt <- CoAt+180
        if (AA>0 && BB<0) CoAt <- CoAt+360
        
        CoAt*points/360
    }
    
    data_all <- data
    order_list <- list()
    
    for (ll in seq_along(data_NMF)) {
        temp <- data_NMF[[ll]]
        cond <- names(data_NMF[ll])
        
        data_NMF_H <- temp$H
        data_NMF_W <- temp$W
        syns_num_n <- ncol(data_NMF_W)
        
        # Order using CoA
        orders <- order(apply(data_NMF_H, 1, CoA))
        
        # # Order using global max
        # orders <- order(apply(data_NMF_H, 1, function(x) which.max(x)[1]))
        
        data_NMF_H <- data_NMF_H[orders, ]
        data_NMF_W <- data_NMF_W[, orders]
        
        # Normalise to 1
        data_NMF_H <- data.frame(t(apply(data_NMF_H, 1, function(x) x/max(x))))
        data_NMF_W <- data.frame(apply(data_NMF_W, 2, function(x) x/max(x)))
        
        rownames(data_NMF_H) <- paste0("Syn", 1:nrow(data_NMF_H))
        colnames(data_NMF_W) <- paste0("Syn", 1:ncol(data_NMF_W))
        
        # Plot classified syns
        # Find plot size
        dev_size <- dev.size(units="in")
        # Margins size are specified in inches following the order:
        # bottom, left, top, right
        # Reduction factor of margins to account for screens at different resolutions
        red_factor <- 35
        par(mfrow=c(syns_num_n, 2),
            mai=c(dev_size[2]/red_factor,
                  dev_size[1]/red_factor,
                  dev_size[2]/red_factor,
                  dev_size[1]/red_factor))
        
        for (syn in 1:syns_num_n) {
            plot(x=c(1:ncol(data_NMF_H)), y=data_NMF_H[syn, ],
                 ty="l", main=paste0("Synergy ", syn, ", ", cond),
                 xlab="", ylab="",
                 xaxt="n", yaxt="n", lwd=2)
            barplot(sort(data_NMF_W[, syn], decreasing=T))
            abline(h=seq(0.2, 0.8, 0.1), col=2)
            tot <- length(data_NMF_W[, syn])
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
            for (cc in 1:nrow(data_NMF_H)) {
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
            
            data_NMF_H <- data_NMF_H[orders, ]
            data_NMF_W <- data_NMF_W[, orders]
            
            # Normalise to 1
            data_NMF_H <- data.frame(t(apply(data_NMF_H, 1, function(x) x/max(x))))
            data_NMF_W <- data.frame(apply(data_NMF_W, 2, function(x) x/max(x)))
            
            rownames(data_NMF_H) <- paste0("Syn", 1:nrow(data_NMF_H))
            colnames(data_NMF_W) <- paste0("Syn", 1:ncol(data_NMF_W))
            
            # Re-plot classified syns
            par(mfrow=c(syns_num_n, 2),
                mai=c(dev_size[2]/red_factor,
                      dev_size[1]/red_factor,
                      dev_size[2]/red_factor,
                      dev_size[1]/red_factor))
            
            for (syn in 1:syns_num_n) {
                plot(x=c(1:ncol(data_NMF_H)), y=data_NMF_H[syn, ],
                     ty="l", main=paste0("Synergy ", syn, ", ", cond),
                     xlab="", ylab="",
                     xaxt="n", yaxt="n", lwd=2)
                barplot(sort(data_NMF_W[, syn], decreasing=T))
                abline(h=seq(0.2, 0.8, 0.1), col=2)
                tot <- length(data_NMF_W[, syn])
                abline(v=tot*seq(0, 1, 0.25), col=2)
            }
            Sys.sleep(2)
        }
        
        # Now search for syns having module bigger than "W_threshold"
        # If a syn has more than one module, choose the one with the highest value
        # Then compare the primitive to the relevant one and assess similarity
        # If similarity is lower than "R2_threshold", classify as combined, otherwise keep
        temp <- data_NMF_W
        
        # Determine the threshold for W and R2
        # Calculate the W threshold
        W_threshold <- mean(colMeans(temp, na.rm=T), na.rm=T)
        
        temp[temp<W_threshold] <- NA
        
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
                H1  <- as.numeric(data_NMF_H[choice, ])
                H2  <- as.numeric(data[tt, ])
                RSS <- sum((H1-H2)^2)
                SST <- sum((H1-mean(H1))^2)
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
                "\n Module threshold: ", round(W_threshold, 2), "\n")
        
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
        
        colnames(SYNS_classified[[trial]]$H) <- c("time", paste0("Syn", classification))
        colnames(SYNS_classified[[trial]]$W) <- paste0("Syn", classification)
    }
    
    message("\nSaving classified synergies...")
    save(SYNS_classified, file=paste0(data_path, "SYNS_classified.RData"))
    message("...done!")
}
