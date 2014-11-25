## gen grouped by "PAT", "MiniPAT", "Mk10" versus "Mk9", "Splash"
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

wc <- function(params, offset = 0) {
  function(Z) {
    offset + params["p2"] * exp(1)^(-exp(1)^((Z - params["p0"]) * params["p1"]))
  }
}

