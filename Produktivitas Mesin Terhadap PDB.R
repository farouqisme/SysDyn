
library(rio)
library(deSolve)
library(FME)
library(ggplot2)


#~~~~~
# di sini coba modelkan perekonomian dengan dua stock utama; mesin dan gdp
# inflow mesin didapatkan dari investasi, outflow dari depresiasi
# inflow gdp dihasilkan dari produksi mesin & labor, outflow dari proporsi yang direinvest
# investasi didapatkan dari fraction output,
# depresiasi dari fraction machine
#~~~~~
  
##set period
T0 <- 2020; TN <- 2100; steps <- 0.25
simtime <- seq(T0,TN,by = steps)

##set stocks
stocks <- c(smesin = 1e2)

##set auxs
auxs <- c(adepre_mesin = 0.05,
          afrac_inves = 0.08,
          labor = 1e1,
          gdp = 1e5
          )



##develop the model
model <- function(time, stocks, auxs){
  with(as.list(c(stocks,auxs)),{
    
    aEconomicOutput <- labor*sqrt(smesin)
    aGDP_oty <- gdp + aEconomicOutput
    
    fInvestasi <- aGDP_oty*afrac_inves
    fDepre_mesin <- smesin * adepre_mesin
    
    dmesin_dt <- fInvestasi - fDepre_mesin
    
    return(list(c(dmesin_dt),
                gdp_of_the_year = aGDP_oty
                ))
  })
}

output_ekonomi <- as.data.frame(ode(
  y = stocks,
  times = simtime,
  func = model,
  parms = auxs
))


ggplot(output_ekonomi, aes(x = time)) +
  geom_line(aes(y = gdp_of_the_year, colour = "PDB Tahunan (Flow)"), size = 1.2) +
  geom_line(aes(y = smesin, colour = "Produksi (Stok)"), size = 1.2) +
  labs(title = "Simulasi Model Ekonomi Ceteris Paribus",
       subtitle = "PDB Tahunan dihitung ulang setiap saat berdasarkan jumlah mesin",
       x = "Tahun", y = "Jumlah Unit") +
  theme_minimal() 

