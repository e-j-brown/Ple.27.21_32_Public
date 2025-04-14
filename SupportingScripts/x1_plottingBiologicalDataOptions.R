#
##
### Call Premade plots
##
#

# Maturity Ogives ----
## Existing Maturity ogives  ----
### Existing combined sexes 15 year recent mean (black),
### Female only 15 year recent mean (blue),
### Female only full assessment period (peach)
ggplotly(plt_mo_rm15FO)


## Fixed maturity ogives based on recent 15 years
### Male = green, combined = orange, female = purple
ggplotly(plt_mo_SW)

## Proposed sliding window maturity ogives ----
### Based on annual survey maturities ----
ggplotly(plt_sw_fmo)

### 3 year ----
ggplotly(plt_mo_sw3)

### 5 year ----
ggplotly(plt_mo_sw5)

## Compare existing to proposed ----
### Existing = combined sex, whole time-series
### Proposed = 3yr sliding window
#### All years in one panel
ggplotly(plt_mo_oldNew)

#### Faceted by year
ggplotly(plt_mo_oldNew_year)


# Stock weights at age ----
## Existing stock weights at age ----


## Proposed sliding window maturity ogives ----
### 3 year ----

### 5 year ----
