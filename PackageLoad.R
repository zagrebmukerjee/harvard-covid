

library(dplyr)
library(plotly)

library(knitr)
library(ggplot2)
library(kableExtra)
library(reshape2)
library(rmarkdown)
library(pdftools)
library(tictoc)


clrs1 <- colorRampPalette(
  c("#003f5c",
    '#58508d',
    "#bc5090",
    "#ff6361"#,
    #"#ffa600"
  )
)(3)


clrs2 <- colorRampPalette(
  c("#4C74C9",
    '#1c424f',
    "#75767d"
  )
)(4)



# need tinytex::install_tinytex()
# need to install Orca