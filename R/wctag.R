##' Parameters for WC tags. 
##' 
##' The tag type \code{gen} is grouped by "PAT", "MiniPAT", "Mk10" versus "Mk9", "Splash"
##' WC tag parameters. By default the offset value is missing and set to zero. This may be 
##' provided explicitly, otherwise may be set in \code{wcparams} component "p3" in future, with 
##' \code{wc} argument \code{offset} set to \code{NULL}. 
##' @title wcparams
##' @param gen tag type
##' @export
wcparams <- function(gen = c("PAT", "Splash"))  {
  gen <- match.arg(gen)
  ## Z = 9 is horizon
  ## p0 is low zenith angle
  ## p1 is gradient
  ## p2 is range
  ## p3 is cloud cover / offset
  switch(gen, 
      PAT = c(p0 = 97.1, p1 = 0.21, p2 = 126, p3 = NA_real_), 
       Splash = c(p0 = 97.1, p1 = 0.35, p2 = 126, p3 = NA_real_))
}

##' @rdname wcparams
##' @param params parameters for WC tag
##' @param offset offset value for additive component, defaults to zero
##' @export
wc <- function(params, offset = 0) {
    if (is.null(offset)) offset <- params["p3"]
  function(Z) {
    offset + params["p2"] * exp(1)^(-exp(1)^((Z - params["p0"]) * params["p1"]))
  }
}

##' Light data from the foraging trips of the Southern elephant seal.
##'
##' Light intensity measurements over time from an archival tag on a
##' Southern elephant seal (\emph{Mirounga leonina}).  The seal was
##' tagged at the isthmus on Macquarie Island (158.950E, 54.5S), with
##' data from a time-depth-recorder (Mk9 TDR; Wildlife Computers,
##' Seattle, WA, USA).  These tags provides regular time series of
##' measurements of depth, water temperature, and ambient light
##' level. The original data for \code{ElephantSeal1} were processed
##' to remove values at depths greater than 15m and to classify
##' periods of twilight. The data for \code{ElephantSeal2} have also
##' been processed for twilight periods. The seals makes one single
##' foraging trip, returning to the isthmus where they were tagged.
##' Data recorded while the seal is at the isthmus are used for
##' calibration (\code{\link{ElephantSeal1calib}}).
##'
##' These data supplied courtesy of Mark Hindell, Institute of Marine
##' and Antarctic Studies, University of
##' Tasmania. \code{ElephantSeal1} is B362_99 and \code{ElephantSeal2}
##' is C699_02
##' @name ElephantSeal1
##' @aliases ElephantSeal1calib ElephantSeal2 ElephantSeal2calib
##' @docType data
##' @title Southern Elephant seal tag data
##' @format \code{ElephantSeal1} A data frame with 3 columns.  The
##' columns represent
##' \tabular{rl}{
##' \code{time} \tab times of measurement \cr
##' \code{light} \tab  (log) light values \cr
##' \code{segment} \tab integer indicating sequence of twilight periods \cr
##' }
##' \code{ElephantSeal2} This tag is similar to \code{ElephantSeal1}
##' but also has columns
##' \tabular{rl}{
##' \code{depth} \tab depth in the water column in metres (positive) \cr
##' \code{temp} \tab temperature in the water column in degrees celcius \cr
##' }
##' \code{ElephantSeal1calib} and \code{ElephantSealcalib2} A data
##' frame with 2 columns.  The columns represent
##' \tabular{rl}{
##' \code{zenith} \tab zenith values \cr
##' \code{light} \tab  (log) light values \cr
##' }
##' @keywords data
NULL
