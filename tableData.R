base_variables <- c('Testing Cost', 'Total Quarantine Entries', 'Total Isolation Entries', 
           'Total Students Symptomatic', 'Accurate Contact Traces', 
           'Peak Isolation Occupancy', 'Total Isolation', 'Total Symptomatic Time',
           'Average Isolation Time per Student', 'Average Symptomatic Time per Student')
base_results <- c(1439258, 0, 211, 10, 0, 7, 5, 117, 0.36, 0.08)

baseline <- data.frame(base_variables, base_results)

party_variables <- c('Testing Cost', 'Total Quarantine Entries', 'Total Isolation Entries', 
               'Total Students Symptomatic', 'Accurate Contact Traces', 
               'Peak Isolation Occupancy', 'Total Isolation', 'Total Symptomatic Time',
               'Average Isolation Time per Student', 'Average Symptomatic Time per Student')
party_results <- c(1439262, 0, 211, 10, 0, 7, 5, 117, 0.36, 0.08)

parties <- data.frame(party_variables, party_results)