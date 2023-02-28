

# setup -------------------------------------------------------------------
source(here::here("r", "libraries.r"))

dnri <- here::here("data", "natural_disasters", "nri")
znri <- "NRI_Table_States.zip"

# get data ----------------------------------------------------------------

df1 <- read_csv(unz(path(dnri, znri), "NRI_Table_States.csv"))
glimpse(df1)
skim(df1)

df2 <- df1 |> 
  select(-STATEFIPS, -NRI_VER) |> 
  rename(oid=OID_, stname=STATE, stabbr=STATEABBRV, pop=POPULATION) |> 
  lcnames()
glimpse(df2)

# split the file into character and numeric and make long
df2c <- df2 |> 
  select(oid:area | where(is.character)) |> 
  pivot_longer(cols=-c(oid:area),
               values_to = "cvalue")

df2n <- df2 |> 
  select(oid:area | where(is.numeric)) |> 
  pivot_longer(cols=-c(oid:area))
hazards <- count(df2n, name)

df3n <- df2n |> 
  separate(name, into=c("hazard", "losstype"), remove=FALSE)
count(df3n, hazard) # 19, not 18??  eal is everything I think
count(df3n, losstype) # also 19 (coincidence?)

hazlabels <- read_csv("
hazlabel, hazard
Avalanche,AVLN
Coastal Flooding,CFLD
Cold Wave,CWAV
Drought,DRGT
Expected annual loss, EAL
Earthquake,ERQK
Hail,HAIL
Heat Wave,HWAV
Hurricane,HRCN
Ice Storm,ISTM
Landslide,LNDS
Lightning,LTNG
Riverine Flooding,RFLD
Strong Wind,SWND
Tornado,TRND
Tsunami,TSUN
Volcanic Activity,VLCN
Wildfire,WFIR
Winter Weather,WNTW
Other, other
Total, total") |> # I added the last two items
  mutate(hazard=str_to_lower(hazard)) |> 
  select(hazard, hazlabel)
hazlabels
saveRDS(hazlabels, path(dnri, "hazlabels.rds"))
  

df4n <- df3n |> 
  left_join(hazlabels, by = join_by(hazard))
skim(df4n)
count(df4n, hazard, hazlabel)
saveRDS(df4n, path(dnri, "nri_numeric.rds"))


# hazard     n
# <chr>  <int>
#   1 avln     510
# 2 cfld     459
# 3 cwav     612
# 4 drgt     306
# 5 eal      357 -- total across all hazards
# 6 erqk     459
# 7 hail     612
# 8 hrcn     612
# 9 hwav     612
# 10 istm     510
# 11 lnds     510
# 12 ltng     510
# 13 rfld     561
# 14 swnd     612
# 15 trnd     612
# 16 tsun     510
# 17 vlcn     510
# 18 wfir     561
# 19 wntw     612

# losstype     n
# <chr>    <int>
# 1 eala       510 -- expected annual loss for indiv hazards, agriculture
# 2 ealb       867      buildings
# 3 ealp       867      people, #
# 4 ealpe      867      people, value
# 5 eals       918   - expected Annual Loss Score
# 6 ealt       918    eal total
# 7 evnts      714 -- expected events
# 8 expa       510 -- exposure
# 9 expb       867
# 10 expp       867
# 11 exppe      867
# 12 expt       918
# 13 npctl       51  national percentile
# 14 score       51
# 15 vala        51 components of ealt (not indiv hazards) by category, agriculture
# 16 valb        51    buildings
# 17 valp        51    people, #
# 18 valpe       51    people, value
# 19 valt        51

# HLR                      Historic Loss Ratio

df3n |> 
  summarise(value=sum(value, na.rm=TRUE), .by=c(hazard)) |> 
  arrange(desc(value))


# total and average loss by state
df3n |> 
  filter(losstype=="ealt") |> 
  mutate(losspc=value / pop) |> 
  arrange(desc(losspc))

df3n |> 
  filter(str_starts(losstype, "val"), losstype!="valp") |> 
  mutate(losspc=value / pop) |> 
  select(stabbr, stname, losstype, losspc) |> 
  pivot_wider(names_from = losstype, values_from = losspc) |> 
  relocate(valt, .after=last_col()) |> 
  arrange(desc(valt))


# loss by hazard
# HRCN_EALB,Hurricane - Expected Annual Loss - Building Value
# HRCN_EALP,Hurricane - Expected Annual Loss - Population
# HRCN_EALPE,Hurricane - Expected Annual Loss - Population Equivalence
# HRCN_EALA,Hurricane - Expected Annual Loss - Agriculture Value
# HRCN_EALT,Hurricane - Expected Annual Loss - Total

totloss <- df3n |> 
  filter(losstype %in% c("ealb", "ealpe", "eala", "ealt")) |> 
  summarise(pop=sum(pop), value=sum(value, na.rm=TRUE), .by=c(hazard, losstype)) |> 
  mutate(losspc=value / pop) |> 
  left_join(hazlabels, by = join_by(hazard)) |> 
  relocate(hazlabel, .after=hazard)
count(totloss, losstype)

totloss |> 
  filter(losstype=="ealt") |> 
  arrange(desc(value))

totloss |> 
  filter(losstype=="ealt") |> 
  select(hazard, value) |> 
  arrange(desc(value)) |> 
  janitor::adorn_totals()

totloss |>
  select(hazard, losstype, value) |> 
  pivot_wider(names_from = losstype) |> 
  select(hazard, eala, ealb, ealpe, ealt) |> 
  arrange(desc(ealt))

# cost per event by type of hazard
eventloss <- df3n |> 
  filter(losstype %in% c("ealt", "evnts")) |> 
  summarize(value=sum(value, na.rm=TRUE), .by=c(hazard, losstype)) |> 
  pivot_wider(names_from = losstype) |> 
  mutate(losspe=ealt / evnts) |> 
  left_join(hazlabels, by = join_by(hazard)) |> 
  relocate(hazlabel, .after=hazard) |> 
  arrange(desc(losspe))
  
eventloss


# table of expected annual loss by hazard, for report -------------------------------------
# get total eal by hazard, for info
tmp <- df3n |> 
  filter(losstype %in% c("ealt")) |> 
  # sum all states
  summarise(value=sum(value, na.rm=TRUE), .by=c(hazard))|>  
  arrange(desc(value)) |>
  left_join(hazlabels, by = join_by(hazard))
# hazard       value hazlabel         
# <chr>        <dbl> <chr>            
#   1 erqk   7196462386. Earthquake       
# 2 trnd   5058725254. Tornado          
# 3 rfld   4719873465. Riverine Flooding
# 4 hrcn   3657501335. Hurricane        
# 5 drgt   3293390470. Drought          
# 6 wfir   1571878222. Wildfire         
# 7 swnd   1276808741. Strong Wind      
# 8 hail   1261439962. Hail             
# 9 cfld   1230073515. Coastal Flooding 
# 10 hwav    877103624. Heat Wave        
# 11 ltng    499817557. Lightning        
# 12 istm    441561591. Ice Storm        
# 13 cwav    428250207. Cold Wave        
# 14 wntw    297297202. Winter Weather   
# 15 lnds    266896652. Landslide        
# 16 vlcn    116574214. Volcanic Activity
# 17 avln     56276854. Avalanche        
# 18 tsun      5172761. Tsunami    



losstab1 <- df3n |> 
  filter(losstype %in% c("ealb", "ealpe", "eala", "ealt")) |> 
  # sum all states
  summarise(pop=sum(pop), value=sum(value, na.rm=TRUE), .by=c(hazard, losstype))|>  
  pivot_wider(names_from = losstype) |> 
  arrange(desc(ealt)) |> 
  mutate(hazard=ifelse(ealt>=1e9, hazard, "other")) |> 
  summarise(pop=first(pop),
            across(starts_with("eal"),
                   ~ sum(.x, na.rm=TRUE)),
            .by=c(hazard))
losstab1

losstots <- losstab1 |> 
  summarise(pop=first(pop), 
            across(starts_with("eal"),
                   ~ sum(.x, na.rm=TRUE))) |> 
  mutate(hazard="total")
losstots

tabdata <- bind_rows(losstab1, losstots) |> 
  mutate(ealtpc=ealt / pop,
         poploss=ealpe / 7600000) |> 
  left_join(hazlabels |> 
              add_row(hazard=c("other", "total"),
                      hazlabel=c("Other", "Total")), by = join_by(hazard)) |> 
  select(hazard, hazlabel, eala, ealb, ealpe, ealt, ealtpc, poploss)
tabdata


tab <- tabdata |> 
  select(-hazard) |> 
  gt() |> 
  tab_header(
    title = "Estimated Annual Loss From Natural Disasters",
    subtitle = "Major Hazards"
  ) |> 
  tab_spanner(columns = c(eala, ealb, ealpe, ealt),
              label="Estimated loss, billions of 2020 $") |> 
  cols_label(hazlabel="Hazard",
             eala = html("Agriculture"),
             ealb = html("Buildings"),
             ealpe = html("Population equivalence"),
             ealt = html("Total"),
             ealtpc = html("Total per capita"),
             poploss = html("Population loss (estimated number)")) |> 
  fmt_number(columns=c(eala, ealb, ealpe, ealt),
             scale=1e-9,
             decimals=2) |> 
  fmt_currency(columns=c(eala, ealb, ealpe, ealt),
               rows=c(1, nrow(tabdata)),
               scale=1e-9,
               decimals=2)|> 
  fmt_number(columns=c(ealtpc, poploss),
             # scale=1e-9,
             decimals=1) |> 
  fmt_currency(columns=c(ealtpc),
               rows=c(1, nrow(tabdata)),
               # scale=1e-9,
               decimals=1)|> 
  # fmt_percent(columns=rentpct, decimals=1) |> 
  gtExtras::gt_highlight_rows(rows = nrow(tabdata),  # needs gtExtras
                    fill="grey97") |>
  tab_footnote(
    footnote = "Calculated by FEMA using a Value of Statistical Life (VSL) approach in which each fatality or ten injuries is treated as $7.6 million of economic loss.",
    locations = cells_column_labels(
      columns = c(ealpe, poploss)
    )
  ) |> 
  # tab_footnote(
  #   footnote = "Heat waves, lightning, ice storms, and cold waves account for 75% of the other category.",
  #   locations = cells_stub(rows=nrow(tabdata))
  # ) |> 
  tab_source_note(source_note="Source: FEMA, National Risk Index Database")
tab






# selected data fields ----------------------------------------------------

# Field Name,Field Alias
# OBJECTID,OBJECTID
# Shape,Shape
# Shape_Length,Shape_Length
# Shape_Area,Shape_Area
# STATE,State Name
# STATEABBRV,State Name Abbreviation
# STATEFIPS,State FIPS Code
# COUNTY,County Name
# COUNTYTYPE,County Type
# COUNTYFIPS,County FIPS Code
# STCOFIPS,State-County FIPS Code
# TRACT,Census Tract
# TRACTFIPS,Census Tract FIPS Code
# NRI_ID,National Risk Index ID
# POPULATION,Population (2016)
# BUILDVALUE,Building Value ($)
# AGRIVALUE,Agriculture Value ($)
# AREA,Area (sq mi)
# RISK_SCORE,National Risk Index - Score - Composite
# RISK_RATNG,National Risk Index - Rating - Composite
# RISK_NPCTL,National Risk Index - National Percentile - Composite
# RISK_SPCTL,National Risk Index - State Percentile - Composite
# EAL_SCORE,Expected Annual Loss - Score - Composite
# EAL_RATNG,Expected Annual Loss - Rating - Composite
# EAL_NPCTL,Expected Annual Loss - National Percentile - Composite
# EAL_SPCTL,Expected Annual Loss - State Percentile - Composite
# EAL_VALT,Expected Annual Loss - Total - Composite
# EAL_VALB,Expected Annual Loss - Building Value - Composite
# EAL_VALP,Expected Annual Loss - Population - Composite
# EAL_VALPE,Expected Annual Loss - Population Equivalence - Composite
# EAL_VALA,Expected Annual Loss - Agriculture Value - Composite
# ...
# ERQK_EVNTS,Earthquake - Number of Events
# ERQK_AFREQ,Earthquake - Annualized Frequency
# ERQK_EXPB,Earthquake - Exposure - Building Value
# ERQK_EXPP,Earthquake - Exposure - Population
# ERQK_EXPPE,Earthquake - Exposure - Population Equivalence
# ERQK_EXPT,Earthquake - Exposure - Total
# ERQK_HLRB,Earthquake - Historic Loss Ratio - Buildings
# ERQK_HLRP,Earthquake - Historic Loss Ratio - Population
# ERQK_HLRR,Earthquake - Historic Loss Ratio - Total Rating
# ERQK_EALB,Earthquake - Expected Annual Loss - Building Value
# ERQK_EALP,Earthquake - Expected Annual Loss - Population
# ERQK_EALPE,Earthquake - Expected Annual Loss - Population Equivalence
# ERQK_EALT,Earthquake - Expected Annual Loss - Total
# ERQK_EALS,Earthquake - Expected Annual Loss Score
# ERQK_EALR,Earthquake - Expected Annual Loss Rating
# ERQK_RISKS,Earthquake - Hazard Type Risk Index Score
# ERQK_RISKR,Earthquake - Hazard Type Risk Index Rating
# ...
# HRCN_EVNTS,Hurricane - Number of Events
# HRCN_AFREQ,Hurricane - Annualized Frequency
# HRCN_EXPB,Hurricane - Exposure - Building Value
# HRCN_EXPP,Hurricane - Exposure - Population
# HRCN_EXPPE,Hurricane - Exposure - Population Equivalence
# HRCN_EXPA,Hurricane - Exposure - Agriculture Value
# HRCN_EXPT,Hurricane - Exposure - Total
# HRCN_HLRB,Hurricane - Historic Loss Ratio - Buildings
# HRCN_HLRP,Hurricane - Historic Loss Ratio - Population
# HRCN_HLRA,Hurricane - Historic Loss Ratio - Agriculture
# HRCN_HLRR,Hurricane - Historic Loss Ratio - Total Rating
# HRCN_EALB,Hurricane - Expected Annual Loss - Building Value
# HRCN_EALP,Hurricane - Expected Annual Loss - Population
# HRCN_EALPE,Hurricane - Expected Annual Loss - Population Equivalence
# HRCN_EALA,Hurricane - Expected Annual Loss - Agriculture Value
# HRCN_EALT,Hurricane - Expected Annual Loss - Total
# HRCN_EALS,Hurricane - Expected Annual Loss Score
# HRCN_EALR,Hurricane - Expected Annual Loss Rating
# HRCN_RISKS,Hurricane - Hazard Type Risk Index Score
# HRCN_RISKR,Hurricane - Hazard Type Risk Index Rating

# note that erqk has 17 lines, hrcn has 20 lines



