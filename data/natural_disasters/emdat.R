

# setup -------------------------------------------------------------------
source(here::here("r", "libraries.r"))

dem <- here::here("data", "natural_disasters")
emfn <- "emdat_public_2023_02_26_query_uid-L7IifF.xlsx"

# get data ----------------------------------------------------------------

df1 <- read_excel(path(dem, emfn), range="a7:ax1494")
glimpse(df1)

df2 <- df1 |> 
  select(disno=`Dis No`, year=Year,
         group=`Disaster Group`, subgroup=`Disaster Subgroup`,
         type=`Disaster Type`, subtype=`Disaster Subtype`,
         subsub=`Disaster Subsubtype`,
         ename=`Event Name`,
         location=Location,
         geo=`Geo Locations`,
         latitude=Latitude, longitude=Longitude,
         deaths=`Total Deaths`,
         rdamage=`Total Damages, Adjusted ('000 US$)`) |> 
  mutate(year=as.integer(year))


df3 <- df2 |> 
  mutate(ycut=cut(year, 10),
         ycut=cut(year, breaks=seq(1880, 2030, 10)),
         ncut=as.integer(ycut)) |> 
  summarise(n=n(), deaths=sum(deaths, na.rm=TRUE),
            rdamage=sum(rdamage, na.rm=TRUE),
            .by=ycut)

df3 |> 
  filter(row_number() %in% 2:13) |> 
  ggplot(aes(ycut, rdamage, group=1)) +
  geom_path()


df2 |>
  summarise(n=n(), deaths=sum(deaths, na.rm=TRUE),
            rdamage=sum(rdamage, na.rm=TRUE),
            .by=c(group, subgroup))

df2 |>
  filter(year>=2000) |> 
  summarise(n=n(), deaths=sum(deaths, na.rm=TRUE),
            rdamage=sum(rdamage, na.rm=TRUE),
            .by=c(type)) |> 
  arrange(desc(rdamage))

df2 |>
  filter(year>=2000) |> 
  summarise(n=n(), deaths=sum(deaths, na.rm=TRUE),
            rdamage=sum(rdamage, na.rm=TRUE),
            .by=c(type, subtype)) |> 
  mutate(rdamtype=sum(rdamage), .by=type) |> 
  arrange(desc(rdamtype), desc(rdamage))

tmp <- df2 |> 
  filter(type=="Earthquake") |> 
  arrange(desc(rdamage))


# severity of events
df2 |> 
  filter(year>=2000) |> 
  summarise(n=n(), 
            deaths_mean=mean(deaths, na.rm=TRUE),
            rdamage_mean=mean(rdamage, na.rm=TRUE),
            deaths_sum=sum(deaths, na.rm=TRUE),
            rdamage_sum=sum(rdamage, na.rm=TRUE),
            .by=c(type)) |> 
  arrange(desc(rdamage_mean))



