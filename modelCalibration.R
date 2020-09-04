
####################################################################################
# average contacts per student per cycle
# based on R0 and prob(infection | contact), and recovery and symptom rate
####################################################################################

recoveryRate <- 1/14
symptomOnsetRate <- recoveryRate*(.3/(1-.3))
divisor <- recoveryRate + symptomOnsetRate
target_r0 = 4
contactInfProb = 0.026
# derive
beta = target_r0*divisor
avgContacts = beta/contactInfProb


####################################################################################
# within pod contacts
# based on pod size, secondary attack rate, prob(infection | contact)
####################################################################################


contactInfProb <- 0.026
hhSecondaryAttackRate <- .15
recoveryRate <- 1/42 # in terms of cycles


hhCycleAttackRate <- recoveryRate * (hhSecondaryAttackRate/(1 - hhSecondaryAttackRate))

podParam <- recoveryRate * (hhSecondaryAttackRate/(1 - hhSecondaryAttackRate))/contactInfProb

podParam <- hhCycleAttackRate/contactInfProb

# sanity check
(1 - (1-podParam)^40)

