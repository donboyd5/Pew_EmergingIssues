

# setup -------------------------------------------------------------------
source(here::here("r", "libraries.r"))

dem <- here::here("data", "natural_disasters")
emfn <- "emdat_public_2023_02_26_query_uid-L7IifF.xlsx"

# get data ----------------------------------------------------------------

df1 <- read_excel(path(dem, emfn), range="a7:ax1494")
glimpse(df1)
