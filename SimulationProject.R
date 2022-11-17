
library(faux)
library(bindata)

mylist.names <- c("suicide", "age", "male", "disability", "depression", 
                   "antidepressants", "benzos", "moodstabilizer", "antipsych", 
                   "sleepingpill", "BMI", "SBP", "DBP", "hgb", "AST", "TG", 
                   "HDL", "LDL", "creatinine")
mylist <- vector("list", length(mylist.names))
names(mylist) <- mylist.names

## Construct a binary correlation matrix
rho <- 0.20
m <- matrix(c(1,rho, rho,rho,1, rho,rho,rho,1), ncol=3)

res <- data.frame(matrix(ncol = 19, nrow = 0))

# Run through simulation results
for (i in 1:10000) {
  for (p in c(0.0001, 0.001, .01, .05)) {
    x <- runif(n = 1, min = 0, max = 1)
    if (x < p) { #suicide
      suicide <- 1
      age <- rnorm(1, mean = 74.1, sd = 5.27)
      male <- rbern(1, 0.72)
      disability <- rbern(1, 0.07)
      #correlation matrix for BMI, BP, Fat, cholesterol measurements
      BMI_BP_FATS <- rnorm_multi(n = 1,
                            mu = c(21.8, 128.3, 77.89, 131.2, 59.47, 107.6),
                            sd = c(2.94, 16.82, 10.57, 83.99, 55.59, 39.08),
                            r = c(0.3, 0.3, 0.1, -0.1, 0.1,
                                  0.6, 0.1, -0.1, 0.1,
                                  0.1, -0.1, 0.1,
                                  -0.5, 0.5,
                                  -0.5),
                            varnames = c("BMI", "SBP", "DBP", "TG", "HDL", "LDL"))
      BMI <- BMI_BP_FATS$BMI
      SBP <- BMI_BP_FATS$SBP
      DBP <- BMI_BP_FATS$DBP
      TG  <- BMI_BP_FATS$TG
      HDL <- BMI_BP_FATS$HDL
      LDL <- BMI_BP_FATS$LDL
      hgb <- rnorm(n = 1, mean = 13.27, sd = 1.8)
      AST <- rnorm(n = 1, mean = 32.17, sd = 36.99)
      creatinine <- rnorm(n = 1, mean = 1.21, sd = 1.13)
      #correlation matrix for depression, antidepressants, benzos
      depdat <- rmvbin(1, margprob = c(0.23, 0.59,0.17), bincorr = m) 
      antidepressants <- depdat[,1]
      benzos <- depdat[,2]
      depression <- depdat[,3]
      moodstabilizer <- rbern(1, 0.04)
      antipsych <- rbern(1, 0.04)
      sleepingpill <- rbern(1, 0.16)
    }
    else { #non-suicide
      suicide <- 0
      age <- rnorm(1, mean = 72.1, sd = 4.41)
      male <- rbern(1, 0.5053)
      disability <- rbern(1, 0.0214)
      #correlation matrix for BMI, BP, Fat, cholesterol measurements
      BMI_BP_FATS <- rnorm_multi(n = 1,
                            mu = c(23.8, 129.8, 78.04, 138.6, 55.17, 116.1),
                            sd = c(3.1, 16.01, 10.02, 82.15, 35.98, 38.74),
                            r = c(0.3, 0.3, 0.1, -0.1, 0.1,
                                  0.6, 0.1, -0.1, 0.1,
                                  0.1, -0.1, 0.1,
                                  -0.5, 0.5,
                                  -0.5),
                            varnames = c("BMI", "SBP", "DBP", "TG", "HDL", "LDL"))
      BMI <- BMI_BP_FATS$BMI
      SBP <- BMI_BP_FATS$SBP
      DBP <- BMI_BP_FATS$DBP
      TG  <- BMI_BP_FATS$TG
      HDL <- BMI_BP_FATS$HDL
      LDL <- BMI_BP_FATS$LDL
      hgb <- rnorm(n = 1, mean = 13.41, sd = 1.45)
      AST <- rnorm(n = 1, mean = 26.55, sd = 15.7)
      creatinine <- rnorm(n = 1, mean = 1.05, sd = 1.11)
      #correlation matrix for depression, antidepressants, benzos
      depdat <- rmvbin(1, margprob = c(0.0854, 0.2869, 0.051), bincorr = m) 
      antidepressants <- depdat[,1]
      benzos <- depdat[,2]
      depression <- depdat[,3]
      moodstabilizer <- rbern(1, 0.012)
      antipsych <- rbern(1, 0.0098)
      sleepingpill <- rbern(1, 0.0377)
    }
      mylist <- c(suicide, p, age, male, disability, depression, antidepressants,
                      benzos, moodstabilizer, antipsych, sleepingpill, BMI,
                      SBP, DBP, hgb, AST, TG, HDL, LDL, creatinine)
     res <- rbind(res, mylist)
  }}

colnames(res) <- c("suicide", "p_suicide", "age", "male", "disability", "depression", 
                   "antidepressants", "benzos", "moodstabilizer", "antipsych", 
                   "sleepingpill", "BMI", "SBP", "DBP", "hgb", "AST", "TG", 
                   "HDL", "LDL", "creatinine")

#correct neg values
res$AST[res$AST < 0] <- 0
res$TG[res$TG < 0] <- 0
res$HDL[res$HDL < 0] <- 0
res$LDL[res$LDL < 0] <- 0
res$creatinine[res$creatinine < 0]  <- 0

#summarize simulated dataset for QC
summary(res)
cor(res$depression, res$antidepressants)
cor(res$depression, res$benzos)
cor(res$BMI, res$LDL)
cor(res$BMI, res$HDL)
cor(res$BMI, res$SBP)
cor(res$BMI, res$TG)
cor(res$SBP, res$TG)
cor(res$SBP, res$HDL)
cor(res$HDL, res$LDL)
