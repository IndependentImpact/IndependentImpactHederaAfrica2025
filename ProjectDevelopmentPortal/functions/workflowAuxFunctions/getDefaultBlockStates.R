# Returns the initial/default states of the blocks in the policy.
#
getDefaultStepStates <- function(nmWrkflwHndlr) {
  return(do.call(what = sprintf("getDefaultStepStates.%s", nmWrkflwHndlr), 
                 args = list()))
}
