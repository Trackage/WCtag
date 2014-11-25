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
  list(PAT = c(p0 = 97.1, p1 = 0.21, p2 = 126, p3 = NA_real_), 
       Splash = c(p0 = 97.1, p1 = 0.35, p2 = 126, p3 = NA_real_))
}

##' @rdname wcparams
##' @param params parameters for WC tag
##' @param offset offset value for additive component, defaults to zero
wc <- function(params, offset = 0) {
  function(Z) {
    if (is.null(offset)) offset <- params["p3"]
    offset + params["p2"] * exp(1)^(-exp(1)^((Z - params["p0"]) * params["p1"]))
  }
}

