library(dplyr)
library(tidyr)

# ==== 1. read in the clean IC file ====
ple_data <- read.csv("ple_data.csv") 

ple_data <- ple_data %>%
filter(CANUM >= 1)

# ==== 2. grouping ====
# all the levels for the grouping: 
gruppen <- c("Year", "Area_large", "Season", "CatchCategory_corrected", "Fleet_corrected", "AgeOrLength")

# ==== 3. Get the weighed average for the weight per age class ====
ple_weighed_avg <- ple_data %>%
  group_by(across(all_of(gruppen))) %>%
  summarise(
    weighed_avg = sum(WECA/1000 * CANUM) / sum(CANUM),  # use CANUM_tsd instead
    summ = sum(CANUM, na.rm = TRUE),  # <- summ up the CANUM
    .groups = "drop"
  ) %>%
# ==== 5. NaN removed ====
filter(!is.nan(weighed_avg))


# === 6. add 10+ as plusgroup ===
zeilen_10p <- ple_weighed_avg %>%
  filter(as.numeric(AgeOrLength) >= 10) %>%
  group_by(Year, Area_large, Season, CatchCategory_corrected, Fleet_corrected) %>%
  summarise(
    AgeOrLength = "10p",
    weighed_avg = sum(weighed_avg * summ, na.rm = TRUE) / sum(summ, na.rm = TRUE),
    summ = sum(summ, na.rm = TRUE),
    .groups = "drop")

# === 7. add 10+ as plusgroup ===
ple_weighed_avg$AgeOrLength <- as.character(ple_weighed_avg$AgeOrLength)
ple_weighed_avg <- bind_rows(ple_weighed_avg, zeilen_10p)


# === 7. aread in the survival rates and add them to the table ===
survival <- read.csv("survival")  

# ==== 7.5 Join ====
combine_ple_surv <- ple_weighed_avg %>%
  left_join(survival, by = c("Area_large", "Fleet_corrected", "Season"))

# ==== 8. get the numbers of dead discards summ * survival_mid ====
combine_ple_surv <- combine_ple_surv %>%
  mutate(
    dead_discard = ifelse(CatchCategory_corrected == "Discards", summ * (1-survival_mid), NA))

# ==== 9. show results and save ====
print(combine_ple_surv)
# write.csv(combine_ple_surv, "combine_ple_surv", row.names = FALSE)


# === 10. merge the table upwards to get the Lowestoft formats
# get rid of the ages above the plusgroup first
combine_ple_surv2 <- combine_ple_surv %>%
  filter(
    AgeOrLength == "10p" |
    (suppressWarnings(as.numeric(AgeOrLength)) < 10 & as.numeric(AgeOrLength) > 0)
  )

# create the basic SAM files (no discard survival) 
cn <- combine_ple_surv2 %>%
  group_by(Year, AgeOrLength) %>%
  summarise(
    summ = sum(summ, na.rm = TRUE),
    .groups = "drop")
  
cw  <- combine_ple_surv2 %>%
  group_by(Year, AgeOrLength) %>%
  summarise(
    weighed_avg = sum(weighed_avg * summ, na.rm = TRUE) / sum(summ, na.rm = TRUE),
    .groups = "drop")
	
lf <- combine_ple_surv2 %>%
  filter(CatchCategory_corrected == "Landings") %>%
  group_by(Year, AgeOrLength) %>%
  summarise(
    summ = sum(summ, na.rm = TRUE),
    .groups = "drop")	
	
lw <- combine_ple_surv2 %>%
  filter(CatchCategory_corrected == "Landings") %>%
  group_by(Year, AgeOrLength) %>%
  summarise(
    weighed_avg = sum(weighed_avg * summ, na.rm = TRUE) / sum(summ, na.rm = TRUE),
    .groups = "drop")	
	
df <- combine_ple_surv2 %>%
  filter(CatchCategory_corrected == "Discards") %>%
  group_by(Year, AgeOrLength) %>%
  summarise(
    summ = sum(summ, na.rm = TRUE),
    .groups = "drop")	
	
dw <- combine_ple_surv2 %>%
  filter(CatchCategory_corrected == "Discards") %>%
  group_by(Year, AgeOrLength) %>%
  summarise(
    weighed_avg = sum(weighed_avg * summ, na.rm = TRUE) / sum(summ, na.rm = TRUE),
    .groups = "drop")		
	
	
########## no creating the version using only dead discards	
# add the dead landings into the column "dead discard"
combine_ple_surv3 <- combine_ple_surv2 %>%
  mutate(
    dead_discard = if_else(is.na(dead_discard), summ, dead_discard)
  )

# now we create the files with the mid_survival applied

cn_mid <- combine_ple_surv3 %>%
  group_by(Year, AgeOrLength) %>%
  summarise(
    summ = sum(dead_discard, na.rm = TRUE),
    .groups = "drop")
  
cw_mid  <- combine_ple_surv3 %>%
  group_by(Year, AgeOrLength) %>%
  summarise(
    weighed_avg = sum(weighed_avg * dead_discard, na.rm = TRUE) / sum(dead_discard, na.rm = TRUE),
    .groups = "drop")
	
lf_mid <- combine_ple_surv3 %>%
  filter(CatchCategory_corrected == "Landings") %>%
  group_by(Year, AgeOrLength) %>%
  summarise(
    summ = sum(dead_discard, na.rm = TRUE),
    .groups = "drop")	
	
lw_mid <- combine_ple_surv3 %>%
  filter(CatchCategory_corrected == "Landings") %>%
  group_by(Year, AgeOrLength) %>%
  summarise(
    weighed_avg = sum(weighed_avg * dead_discard, na.rm = TRUE) / sum(dead_discard, na.rm = TRUE),
    .groups = "drop")	
	
df_mid <- combine_ple_surv3 %>%
  filter(CatchCategory_corrected == "Discards") %>%
  group_by(Year, AgeOrLength) %>%
  summarise(
    summ = sum(dead_discard, na.rm = TRUE),
    .groups = "drop")	
	
dw_mid <- combine_ple_surv3 %>%
  filter(CatchCategory_corrected == "Discards") %>%
  group_by(Year, AgeOrLength) %>%
  summarise(
    weighed_avg = sum(weighed_avg * dead_discard, na.rm = TRUE) / sum(dead_discard, na.rm = TRUE),
    .groups = "drop")		
	
#### make the lf table a fraction table

lf_frac <- lf %>%
  left_join(cn, by = c("Year", "AgeOrLength"), suffix = c("_lf", "_cn")) %>%
  mutate(
    fraction = `summ_lf` / `summ_cn`  # divide total numbers
  ) %>%
  select(Year, AgeOrLength, fraction)

lf_mid_frac <- lf_mid %>%
  left_join(cn_mid, by = c("Year", "AgeOrLength"), suffix = c("_lf", "_cn")) %>%
  mutate(
    fraction = `summ_lf` / `summ_cn`  # divide total numbers
  ) %>%
  select(Year, AgeOrLength, fraction)


#### transform to Lowestoft
table_names <- c("cn", "df", "lf", "cn_mid", "df_mid", "lf_mid")

#### put 10plus group at the end
for (tbl_name in table_names) {
  
  # Tabelle holen
  tbl <- get(tbl_name)
  
  # Umstrukturieren: AgeOrLength als Spalten, "10p" ans Ende
  tbl <- tbl %>%
    pivot_wider(
      names_from = AgeOrLength,
      values_from = summ
    ) %>%
    mutate(across(where(is.numeric), ~replace_na(., -9))) %>%  # make NA to -9
    relocate(`10p`, .after = last_col())  # move plusgroup to the end
  
  # put them back in the global environment
  assign(paste0(tbl_name), tbl)
}	

#######################################################################
table_names <- c("cw", "dw", "lw", "cw_mid", "dw_mid", "lw_mid")

# widen the tables in a loop 
for (tbl_name in table_names) {
  
  # Tabelle holen
  tbl <- get(tbl_name)
  
  # Umstrukturieren: AgeOrLength als Spalten, "10p" ans Ende
  tbl <- tbl %>%
    pivot_wider(
      names_from = AgeOrLength,
      values_from = weighed_avg
    ) %>%
    mutate(across(where(is.numeric), ~replace_na(., -9))) %>%  # make NA to -9
    relocate(`10p`, .after = last_col())  # move plusgroup to the end
  
  # put them back in the global environment
  assign(paste0(tbl_name), tbl)
  }	

#######################################################################
table_names <- c("lf_frac", "lf_mid_frac")

# Durch jede Tabelle iterieren
for (tbl_name in table_names) {
  
  # get table
  tbl <- get(tbl_name)
  
  # put 10plus group at the end
  tbl <- tbl %>%
    pivot_wider(
      names_from = AgeOrLength,
      values_from = fraction
    ) %>%
    relocate(`10p`, .after = last_col())  # 10p ans Ende verschieben
  
  # put them back in the global environment
  assign(paste0(tbl_name), tbl)
  }	


####### Lowestoft format ######
# table names
tables <- c("cn", "cw", "df", "dw", "lf", "lw", "cn_mid", "cw_mid", "df_mid", "dw_mid", "lf_mid", "lw_mid", "lf_frac", "lf_mid_frac")

# loop them and delete the year column
for (tbl_name in tables) {
  
  # get the table
  tbl <- get(tbl_name)
  
  # remove years
  tbl_cleaned <- tbl[, -1]
  
  # put them back in the global environment
  assign(tbl_name, tbl_cleaned)
}

  
### add header for each of the files 
tables <- c("cn", "cw", "df", "dw", "lf", "lw", "cn_mid", "cw_mid", "df_mid", "dw_mid", "lf_mid", "lw_mid", "lf_frac", "lf_mid_frac")

for (i in seq_along(tables)) {
  tbl_name <- tables[i]
  tbl <- get(tbl_name)
  
  # set numbers of rows based on tables
  n_rows <- nrow(tbl)
  n_cols <- ncol(tbl)
  
  # add empty row, no reptition per column
  empty_row <- function() {
    as.data.frame(matrix("", nrow = 1, ncol = n_cols), stringsAsFactors = FALSE)
  }
  
  # 1. Title
  row1 <- empty_row()
  row1[1, 1] <- paste("ple.27.21-32:", tbl_name)
  
  # 2. numbering
  row2 <- empty_row()
  row2[1, 1:2] <- c("1", as.character(i))
  
  # 3. year range
  row3 <- empty_row()
  row3[1, 1:2] <- c("2002", as.character(2002 + n_rows-1))
  
  # 4. age range
  row4 <- empty_row()
  row4[1, 1:2] <- c("1", "10")
  
  # 5. table dimension
  row5 <- empty_row()
  row5[1, 1] <- "1"
  
  # combine
  colnames(row1) <- colnames(tbl)
  colnames(row2) <- colnames(tbl)
  colnames(row3) <- colnames(tbl)
  colnames(row4) <- colnames(tbl)
  colnames(row5) <- colnames(tbl)
  
  # final combination
  tbl_with_header <- rbind(row1, row2, row3, row4, row5, tbl)
  
  # put them back in the global environment
  assign(paste0(tbl_name), tbl_with_header)
  
  # save as csv
#  write.csv(tbl_with_header, paste0(tbl_name, "_final.csv"), row.names = FALSE, quote = FALSE)
}

## Save all results as csv 
# Table overview
tables <- c("cn", "cw", "df", "dw", "lf", "lw", "cn_mid", "cw_mid", "df_mid", "dw_mid", "lf_mid", "lw_mid", "lf_frac", "lf_mid_frac")

#  round the valus from line 6
for (table_name in tables) {
  table_data <- get(table_name)
  
  # make them numerical
  table_data[6:nrow(table_data), ] <- lapply(table_data[6:nrow(table_data), ], function(col) {
    if (is.character(col)) {
      col <- suppressWarnings(as.numeric(col))  # Versuche die Spalte in numerisch zu konvertieren
    }
    if (is.numeric(col)) {
      round(col, 3)  # round
    } else {
      col  # leave header 
    }
  })
  
  # put them back in the global environment
  assign(paste0(table_name), table_data)
}


# Save tables as single .dat
for (table_name in tables) {

    table_data <- get(table_name)
	
    # save .dat 
    write.table(table_data,
                file = paste0(table_name, ".dat"),
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)   
}	




