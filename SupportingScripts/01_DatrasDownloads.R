#
##
###  This script connects to DATRAS to download Datras exchange products
###  It then stores them as .csv for downstream use 
##
#

#===
# Dependencies ----
#===
library(icesDatras)
#====

#===
# Download and save NS-IBTS Exchange products as CSV (Only needed for data update) ----
#===

## Specify data for download
assYear <- as.integer(format(Sys.Date(), "%Y"))
survey <- "NS-IBTS"
years <- 1965:assYear
Q <- c(1,3)
savePath <- paste0("../", assYear, "/Surveys/")

## Download HH, HL & CA Data products independently
HH_ibts <- getDATRAS(record = "HH",
                survey = survey,
                years = years,
                quarters = Q)

HL_ibts <- getDATRAS(record = "HL",
                survey = survey,
                years = years,
                quarters = Q)

CA_ibts <- getDATRAS(record = "CA",
                survey = survey,
                years = years,
                quarters = Q)

##  Save all as independent csv files - using rstudio project console
write.csv(HH_ibts, file = paste0(savePath, "IBTS_HH_1965_", assYear, "_", Sys.Date(),".csv"), row.names = FALSE)
write.csv(HL_ibts, file = paste0(savePath, "IBTS_HL_1965_", assYear, "_", Sys.Date(),".csv"), row.names = FALSE)
write.csv(CA_ibts, file = paste0(savePath, "IBTS_CA_1965", assYear, "_", Sys.Date(),".csv"), row.names = FALSE)

# ##  Save all as independent csv files - using source as local job
# write.csv(HH_ibts, file = "Surveys/exchange/IBTS_HH_1965_AssYear.csv", row.names = FALSE)
# write.csv(HL_ibts, file = "Surveys/exchange/IBTS_HL_1965_AssYear.csv", row.names = FALSE)
# write.csv(CA_ibts, file = "Surveys/exchange/IBTS_CA_1965_AssYear.csv", row.names = FALSE)
#====

#===
# Download and save BITS Exchange products as CSV (Only needed for data update) ----
#===

## Specify data for download
assYear <- as.integer(format(Sys.Date(), "%Y"))
survey <- "BITS"
years <- 1991:assYear
Q <- c(1,4)
savePath <- paste0("../", assYear, "/Surveys/")


## Download HH, HL & CA Data products independently
HH_bits <- getDATRAS(record = "HH",
                survey = survey,
                years = years,
                quarters = Q)

HL_bits <- getDATRAS(record = "HL",
                survey = survey,
                years = years,
                quarters = Q)

CA_bits <- getDATRAS(record = "CA",
                survey = survey,
                years = years,
                quarters = Q)

##  Save all as independent csv files - run in rstudio project
write.csv(HH_bits, file = paste0(savePath, "BITS_HH_1991_", assYear, "_", Sys.Date(),".csv"), row.names = FALSE)
write.csv(HL_bits, file = paste0(savePath, "BITS_HL_1991_", assYear, "_", Sys.Date(),".csv"), row.names = FALSE)
write.csv(CA_bits, file = paste0(savePath, "BITS_CA_1991_", assYear, "_", Sys.Date(),".csv"), row.names = FALSE)

# ##  Save all as independent csv files - run as local job
# write.csv(HH_bits, file = "Surveys/exchange/BITS_HH_1991_AssYear.csv", row.names = FALSE)
# write.csv(HL_bits, file = "Surveys/exchange/BITS_HL_1991_AssYear.csv", row.names = FALSE)
# write.csv(CA_bits, file = "Surveys/exchange/BITS_CA_1991_AssYear.csv", row.names = FALSE)
#====