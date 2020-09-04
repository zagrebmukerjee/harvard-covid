
analyse_pod_contacts  = function(podSize, n_total, 
                                 avg_daily_contacts, withinPodContact=1, diag_value=0){
  # assert divisibility
  stopifnot(n_total %% podSize == 0)
  # signal what pod amplification
  n_pods = n_total/podSize
  blocks = rep(list(matrix(withinPodContact, podSize, podSize)), n_pods) # list of blocks of    "withinPodContact"
  A = bdiag(blocks) # builds blocks into a single matrix
  # assign remaining values:
  A[A==0] = avg_daily_contacts/(n_total-podSize)
  diag(A) <- diag_value  # replace diagonal
  # assert sum by rows is correct

  return(list(matrix = A)) 
}

contactMatrixTransmission <- function(invariants, partySize){
  
  # invariants <- testParameters$timeInvariantParams
  
  # contactMatrix <- analyse_pod_contacts(invariants$podSize, invariants$studentPopulation, invariants$avgNonPodContacts)$matrix 
  # newR0 <- invariants$conditionalInfectionProb*sum(contactMatrix)/
  #   invariants$studentPopulation/(invariants$recoveryRate + invariants$symptomOnsetRate)
  # contactMatrixSum <- sum(contactMatrix)
  
  effectiveContacts <- invariants$avgNonPodContacts + (partySize* invariants$contactsPerParty)/invariants$studentPopulation
  
  # for now - for speed - closed form of sum of contact matrix
  # contactMatrixSum <- (invariants$studentPopulation*invariants$podSize - invariants$studentPopulation) + invariants$avgNonPodContacts*invariants$studentPopulation
  contactMatrixSum <- invariants$studentPopulation*(effectiveContacts + (invariants$podSize-1)*invariants$intraPodContacts)
  
  newR0 <- invariants$conditionalInfectionProb*contactMatrixSum/
    invariants$studentPopulation/(invariants$recoveryRate + invariants$symptomOnsetRate)
  newBeta <- newR0*(invariants$recoveryRate + invariants$symptomOnsetRate)
  # print(round(newBeta - invariants$asymptomaticTransRate,5))
  return(newBeta)

}