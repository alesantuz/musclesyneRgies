#' Raw EMG example
#'
#' A list containing electromyographic (EMG) human data from the right-side
#' lower limb recorded during one walking trial.
#'
#' @format A list containing one object of class `EMG`
#' with elements `cycles` and `emg`, both data frames.
#' \describe{
#'   \item{`ID0012_TW_01`}{Object of class `EMG` containing the two following data frames:}
#'   \item{`cycles`}{Gait cycle-timings, in seconds.}
#'   \item{`emg`}{Raw EMG, first column is time in seconds, muscles named as:\cr\cr
#'          ME=gluteus medius\cr
#'          MA=gluteus maximus\cr
#'          FL=tensor fasciae latae\cr
#'          RF=rectus femoris\cr
#'          VM=vastus medialis\cr
#'          VL=vastus lateralis\cr
#'          ST=semitendinosus\cr
#'          BF=biceps femoris\cr
#'          TA=tibialis anterior\cr
#'          PL=peroneus longus\cr
#'          GM=gastrocnemius medialis\cr
#'          GL=gastrocnemius lateralis\cr
#'          SO=soleus}
#' }
#' @source \doi{10.1016/j.isci.2019.100796}
"RAW_DATA"

#' Filtered EMG example
#'
#' A list containing filtered and time-normalised electromyographic (EMG)
#' human data from the right-side lower limb recorded during one walking trial.
#'
#' @format A list containing one object of class `EMG`
#' with elements `cycles` and `emg`, both data frames.
#' \describe{
#'   \item{`ID0012_TW_01`}{Object of class `EMG` containing the two following data frames:}
#'   \item{`cycles`}{Gait cycle-timings, in seconds.}
#'   \item{`emg`}{Filtered and time-normalised EMG, first column is time in points, muscles named as:\cr\cr
#'          ME=gluteus medius\cr
#'          MA=gluteus maximus\cr
#'          FL=tensor fasciae latae\cr
#'          RF=rectus femoris\cr
#'          VM=vastus medialis\cr
#'          VL=vastus lateralis\cr
#'          ST=semitendinosus\cr
#'          BF=biceps femoris\cr
#'          TA=tibialis anterior\cr
#'          PL=peroneus longus\cr
#'          GM=gastrocnemius medialis\cr
#'          GL=gastrocnemius lateralis\cr
#'          SO=soleus}
#' }
#' @source \doi{10.1016/j.isci.2019.100796}
"FILT_EMG"

#' Muscle synergies example
#'
#' A list created by synsNMF containing muscle synergies extracted from
#' 15 humans walking on a treadmill.
#'
#' @format A list containing 15 objects of class `musclesyneRgies`, each of which represents
#' a walking trial from a different person.
#'
#' \describe{
#'   \item{`ID0012_TW_01`\cr
#'   `ID0001_TW_01`\cr
#'   `ID0002_TW_01`\cr
#'   `ID0003_TW_01`\cr
#'   `ID0004_TW_01`\cr
#'   `ID0005_TW_01`\cr
#'   `ID0006_TW_01`\cr
#'   `ID0007_TW_01`\cr
#'   `ID0008_TW_01`\cr
#'   `ID0009_TW_01`\cr
#'   `ID0010_TW_01`\cr
#'   `ID0011_TW_01`\cr
#'   `ID0012_TW_01`\cr
#'   `ID0013_TW_01`\cr
#'   `ID0014_TW_01`\cr
#'   `ID0015_TW_01`\cr}{Objects of class `musclesyneRgies` containing the following items:}
#'   \item{`syns`}{Factorisation rank or minimum number of synergies.}
#'   \item{`M`}{Motor modules (time-invariant coefficients)}
#'   \item{`P`}{Motor primitives (time-dependent coefficients)}
#'   \item{`V`}{Original data, muscles named as:\cr\cr
#'          ME=gluteus medius\cr
#'          MA=gluteus maximus\cr
#'          FL=tensor fasciae latae\cr
#'          RF=rectus femoris\cr
#'          VM=vastus medialis\cr
#'          VL=vastus lateralis\cr
#'          ST=semitendinosus\cr
#'          BF=biceps femoris\cr
#'          TA=tibialis anterior\cr
#'          PL=peroneus longus\cr
#'          GM=gastrocnemius medialis\cr
#'          GL=gastrocnemius lateralis\cr
#'          SO=soleus}
#'   \item{`Vr`}{Reconstructed data, muscles named as in `Vr`}
#'   \item{`iterations`}{Number of iterations to convergence}
#'   \item{`R2`}{Quality of reconstruction (coefficient of determination)}
#'   \item{`classification`}{Classification type (e.g., `none`, `k-means`, `NMF`, etc.)}
#' }
#' @source \doi{10.1016/j.isci.2019.100796}
"SYNS"

#' Single motor primitive example (30 cycles)
#'
#' A data frame containing one motor primitive extracted from one wild type mouse
#' walking on a treadmill.
#'
#' @format A data frame of two columns:
#'
#' \describe{
#'   \item{`time`}{Normalised time in points.}\cr
#'   \item{`signal`}{Motor primitive}
#' }
#' @source \doi{10.1152/jn.00360.2020}
"primitive"

#' All motor primitives of one synergy example (30 cycles)
#'
#' A demo and incomplete `musclesyneRgies` object containing time info and three motor primitives
#' extracted from one wild type mouse walking on a treadmill.
#'
#' @format A data frame of four columns:
#'
#' \describe{
#'   \item{`time`}{Normalised time in points.}\cr
#'   \item{`Syn1`}{Motor primitive of synergy 1}\cr
#'   \item{`Syn2`}{Motor primitive of synergy 2}\cr
#'   \item{`Syn3`}{Motor primitive of synergy 3}
#' }
#' @source \doi{10.1152/jn.00360.2020}
"primitives"
