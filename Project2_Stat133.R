#Part 1

#Which California county had the most ZEV sales in 2025?
#Which vehicle manufacturer had the most ZEV sales in California in 2023 vs. 2025?
#Which ZEV make experienced the largest increase in sales from 2020 to 2025?

#Part 2
install.packages("readxl")

library(readxl)

zev <- read_excel(
  "data/New_ZEV_Sales_Last_updated_07-17-2026_ada.xlsx",
  sheet = "County"
)

View(zev)

names(zev)
dim(zev)
head(zev)

#does this data record new ZEV sales? Or new purchases? Or new registrations? 

#estimated new ZEV sales derived from Cal DMV registration data, rather than records of purchases at dealerships.
#a registration isn't necessarily = new vehicle sold
#2021 onwards, CEC identifies new sale when vehicle has less than 120 miles on its odometer and model year is within 1 year before or after registration year. Also uses transaction log to prevent older vehicles from being counted twice
#source is registration data, stat is estimated new-vehicle sales

#Unit Observation:Row represents aggregated count of vehicles for a particular combo of year, quarter, county, fuel type, make ,model.

#part 3

library(dplyr)

#3.1
zev |>
  summarize(total_sales = sum(`Number of Vehicles`))

#3.2
zev |>
  filter(`Data Year` == 2026) |>
  summarize(total_sales = sum(`Number of Vehicles`))



#3.3
zev |>
  filter(`Data Year` == 2026, COUNTY == "San Mateo") |>
  summarize(total_sales = sum(`Number of Vehicles`))

#3.4
zev |>
  filter(`Data Year` == 2026) |>
  group_by(FUEL_TYPE) |>
  summarize(total_sales = sum(`Number of Vehicles`))
          
#3.5
# To calculate this share, I'd need both ZEV sales and total light-duty vehicle sales to calculate ZEV share
#ZEV shares = ZEV sales / all light-duty vehicle sales

#3.6
zev |>
  filter(`Data Year` == 2026, MAKE == "Tesla") |>
  group_by(MODEL) |>
  summarize(total_sales = sum(`Number of Vehicles`))
  
#3.7
zev |>
  filter(`Data Year` == 2026) |>
  group_by(MAKE, MODEL) |>
  summarize(total_sales = sum(`Number of Vehicles`))


#Part 4

#B). The previous federal administration put in place financial incentives to encourage 
#people to purchase electric vehicles. Those incentives ended on September 30, 2025. 
#Is there a Q3 2025 surge followed by a Q4 drop, and does it differ between BEVs and PHEVs?
zev |>
  filter(`Data Year` == 2025,
         FUEL_TYPE %in% c("Electric", "PHEV")) |>
  group_by(Quarter, FUEL_TYPE) |>
  summarize(total_sales = sum(`Number of Vehicles`))

#ANSWER: Q3 Elective Vehicle spike, PHEV stay same, Q4 both plummeted. Could be misleading bc there could have been other factors that influecned this,
#like broder EV trends, seasonal purchasing pattnrs, supply of vehicles.


#C).Market Shares, higher HHI means more concentrated less competition

make_sales <- zev |>
  group_by(`Data Year`, MAKE) |>
  summarize(sales = sum(`Number of Vehicles`))

make_sales

hhi_yearly <- make_sales |>
  group_by(`Data Year`) |>
  mutate(share = sales / sum(sales)) |>
  summarize(HHI = sum(share^2))

hhi_yearly

#since 2009 the ZEV has become less concentrated. Concentration decreased drasticallyl from 2009-2017, then 2017-22 inccreased, then 2022-now back down


#A). #Which ZEV make experienced the largest increase in sales from 2020 to 2025?

zev |>
  filter(`Data Year` %in% c(2020, 2025)) |>
  group_by(`Data Year`, MAKE) |>
  summarize(
    sales = sum(`Number of Vehicles`)
  )

sales_2020 <- zev |>
  filter(`Data Year` == 2020) |>
  group_by(MAKE) |>
  summarize(sales_2020 = sum(`Number of Vehicles`))

sales_2025 <- zev |>
  filter(`Data Year` == 2025) |>
  group_by(MAKE) |>
  summarize(sales_2025 = sum(`Number of Vehicles`))

increase <- zev |>
  filter(`Data Year` %in% c(2020, 2025)) |>
  group_by(MAKE) |>
  summarize(
    sales_2020 = sum(`Number of Vehicles`[`Data Year` == 2020]),
    sales_2025 = sum(`Number of Vehicles`[`Data Year` == 2025]),
    increase = sales_2025 - sales_2020
  ) |>
  arrange(desc(increase))

increase

#ANSWER: TESLA
