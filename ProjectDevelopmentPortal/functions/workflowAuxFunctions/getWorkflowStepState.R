# Auxiliary function used by workflow modules.
#
# Determines a workflow step's state, based on the documents already submitted 
# for the step. 
# Returns one of the following character values: 
#  - START
#  - IN_PROGRESS
#  - PENDING_REVIEW
#  - RETRY
#  - DONE
#
getWorkflowStepState <- function(dfStepDocs) {
  
  if (nrow(dfStepDocs) == 0) { return('START') }
  
  # Summarise all docs (except drafts).
  # Note: 'outcome' and 'status' are variables in our PostgreSQL db, not
  # necessarily variables inside the documents themselves.
  dfDocSumm <- data.frame( 
    n_approved = length(which(dfStepDocs$outcome %in% c("APPROVED", "REVIEWED", "ISSUED"))),
    n_rejected = length(which(dfStepDocs$outcome %in% c("REJECTED", "REVOKED"))),
    n_pending = length(which(dfStepDocs$outcome == "PENDING")),
    n_draft = length(which(dfStepDocs$status == 'DRAFT')),
    n_published = length(which(dfStepDocs$status %in% c("PUBLISHED", "SUBMITTED"))))
  
  # Determine step state.
  
  if (dfDocSumm$n_published == 0) { 
    
    # No PRIMARY_AGENT has published/submitted a document of the required schema 
    # for this step yet, so they could either start filling out a document of
    # the correct schema (from scratch), or continue filling out a draft that 
    # they had saved earlier.
    
    if (dfDocSumm$n_draft > 0) {
      return('IN_PROGRESS')
    }
    
    return('START')
  } 
  
  # Reaching this point, means at least one PRIMARY_AGENT has already 
  # published/submitted at least one document of the correct schema to this 
  # step, so all PRIMARY_AGENTs should either wait for the submitted 
  # document(s) to be reviewed by the relevant authority, or try submitting a 
  # document again if the previous one(s) had been reviewed but rejected.
  
  if (dfDocSumm$n_pending > 0) {
    
    # PRIMARY_AGENT must wait for their published (submitted) document to be 
    # reviewed by the relevant authority.
    
    return('PENDING_REVIEW')
  } 
  
  if (dfDocSumm$n_approved == 0) {
    
    # PRIMARY_AGENT published a document for this step, but it has been 
    # rejected. They can now retry - either by continuing with a save draft or
    # by starting with a document for this step from scratch.
    
    if (dfDocSumm$n_draft > 0) {
      return('IN_PROGRESS')
    }
    
    return('RETRY')
  }
  
  # PRIMARY_AGENT published a document for this step and it has been approved,
  # so this step of the workflow has been completed.
  
  return('DONE')
  
}
