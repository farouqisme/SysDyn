
library(rio)
library(deSolve)
library(FME)
library(ggplot2)


T0 <- 2020; TN <- 2150; steps <- 1
simtime <- seq(T0,TN,by = steps)


stocks <- c(sExtrCapacity     = 5.5e5,   #Metric Tone/yr
            sSmeltingCapacity = 5.0e5,   #Metric Tone/yr
            sTechExtrEff      = 0.75,   #Coefficient
            sTechSmltEff      = 0.85,   #Coefficient
            sRawNickel        = 55e6,  #Metric Ton
            sOperatingProfit  = 1 #IDR  
            ) 


auxs <- c(
  # Financial & Economics Params
  aPrice             = 247e6, #IDR/Ton
  aExtrCost          = 53e6,  #IDR
  aSmltCost          = 72e6, #IDR
  aPropForInvest     = 0.1,    #% from profit
  
  # Investment & Cost to Invest
  aInvestExtrEff     = 6e11,  #IDR
  aInvestSmltEff     = 7e11,  #IDR
  aCostToBuildExtrCap= 70e6, #IDR
  aCostToBuildSmltCap= 90e6, #IDR
  
  # Balancing Loop
  aExtrCapLifetime   = 20, #40qtr
  aSmltCapLifetime   = 25, #40qtr
  aDepre.ExtrTechEff = 0.05, #%
  aDepre.SmltTechEff = 0.07 #%
)



x.Resource.key <- c(0, 5.5e6, 27.5e6, 55e6) # Beberapa titik kunci
y.Efficiency.key <- c(0.1, 0.4, 0.9, 1.0)     # Efisiensi di titik kunci

func.Extr.Efficiency <- approxfun(x = x.Resource.key, y = y.Efficiency.key, rule = 2)

model <- function(time, stocks, auxs) {
  with(as.list(c(stocks, auxs)), {
    
    #Kapasitas tambang dengan constraint geologi & teknologi
    aGeogEff       <- func.Extr.Efficiency(sRawNickel)
    aTotalExtrEff  <- aGeogEff * sTechExtrEff
    aAmountMined   <- sExtrCapacity * aTotalExtrEff
    aReadytoSmelt  <- min(aAmountMined, sSmeltingCapacity)
    aAmountSmelted <- aReadytoSmelt * sTechSmltEff
    
    
    #Perhitungan finansial
    aRevenue       <- aAmountSmelted * aPrice
    
    aTotExtrCost   <- aAmountMined * aExtrCost
    aTotSmltCost   <- aAmountSmelted * aSmltCost
    
    aCOGS          <- aTotExtrCost + aTotSmltCost
    aProfit        <- aRevenue - aCOGS
    
    #########TERDAPAT MASALAH DALAM PENENTUAN INVESTMENT
    #########HASILNYA sSmeltingCapacity menjadi minus dan aInvest
    #########Menjadi 0 sehingga perhitungan tidak terjadi!
    aInvest        <- max(0, sOperatingProfit * aPropForInvest)
    
    #Distributing Investments
    if (is.na(sSmeltingCapacity) || sSmeltingCapacity > 0) {
      
      if (is.na(sSmeltingCapacity)) {
        browser()
      }
      aPropEff <- 1
    } else {
      aPropEff <- sExtrCapacity / sSmeltingCapacity
    }
    
    if(aPropEff >= 2){
      propInvest <- 1
    } else if(aPropEff > 1 & aPropEff < 2){
      propInvest <- 0.8
    } else if(aPropEff == 1){
      propInvest <- 0.5
    } else if(aPropEff > 0.5 & aPropEff <= 1){
      propInvest <- 0.2
    } else{
      propInvest <- 0
    }
    
    aInvestToSmlt <- aInvest * propInvest
    aInvestToExtr <- aInvest * (1-propInvest)
    
    
    #Add Investments for Inflows and Outflows
    fImprove.ExtrTechCap <- aInvestToExtr / aCostToBuildExtrCap
    fLoseExtrCap  <- sExtrCapacity / aExtrCapLifetime
    
    fImprove.SmltTechCap <- aInvestToSmlt / aCostToBuildSmltCap
    fLoseSmltCap  <- sSmeltingCapacity / aSmltCapLifetime
    
    fImprove.ExtrTechEff <- aInvestToExtr / aInvestExtrEff
    fDepre.ExtrTechEff <- sTechExtrEff * aDepre.ExtrTechEff
    
    fImprove.SmltTechEff <- aInvestToSmlt / aInvestSmltEff
    fDepre.SmltTechEff <- sTechSmltEff * aDepre.SmltTechEff
    

    
    dEC_dt <- fImprove.ExtrTechCap - fLoseExtrCap
    dSC_dt <- fImprove.SmltTechCap - fLoseSmltCap
    dEx_dt <- fImprove.ExtrTechEff - fDepre.ExtrTechEff
    dSm_dt <- fImprove.SmltTechEff - fDepre.SmltTechEff
    dRN_dt <- -aAmountMined
    dOP_dt <- aProfit - aInvest
    
    return(list(c(dEC_dt, dSC_dt, dEx_dt, dSm_dt, dRN_dt, dOP_dt),
                Produksi_Nikel_Mentah = aAmountMined,
                Profit_Tahunan = aProfit,
                Investment_Tahunan = aInvest
                ))
  })
}


model_nickel <- data.frame(ode(y = stocks, time = simtime, func = model,
                               parms = auxs, method = "euler"))



ggplot(model_nickel, aes(x = time)) +
  geom_line(aes(y = sSmeltingCapacity, colour = "Capital"), size = 1.2) +
  labs(title = "Simulasi Model Ekonomi Ceteris Paribus",
       subtitle = "PDB Tahunan dihitung ulang setiap saat berdasarkan jumlah mesin",
       x = "Tahun", y = "Jumlah Unit") +
  theme_minimal() 
