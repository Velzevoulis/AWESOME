# Clear environment ------------------------------------------------------------------------------
rm(list = ls())

# Set working directory --------------------------------------------------------------------------
wd <- "F:/AWESOME/ES_Valuation/"
setwd(wd)

# Load necessary files
file.name <- paste0(wd,"paper_data.csv")
X <- read.csv(file.name)
X <- as.data.frame(X)

year <- seq(1987,2018,by=1)

####################################
##                                ##  
##  MODELING APPROACHES - EGYPT   ##
##                                ##
####################################

## Step-0 : Perform interpolation for water stress data ======================

X.EGY <- X[X$Country=="EGY",] 
id <- !is.na(X.EGY$Water_stress_level)
wat <- X.EGY$Water_stress_level[id]
xi <- year[id]
yi <- wat
spl <- spline(xi,yi,xout=year)
water <- spl$y

attach(X.EGY)

max.cropland     <- 4200
cropland.ratio   <- Cropland/max.cropland
log.land.std     <- log( cropland.ratio)
log.land.used    <- log( Cropland_used/1000)     # in million ha
log.crops.prod   <- log( prod_crops_qty/1e+06)   # in millions tonnes
log.lvstock.prod <- log( prod_lvstock_qty/1e+06) # in millions tonnes
log.veget.prod   <- log( prod_veg_qty/1e+06)     # in millions tonnes
log.crops.gpv    <- log( gpv_crops/1e+06)        #scale to billions USD
log.lvstock.gpv  <- log( gpv_lvstock/1e+06)      #scale to billions USD
log.veget.gpv    <- log( gpv_veg/1e+06)          #scale to billions USD
log.pop          <- log( Population_total/1000)  # in thousands persons
log.GDP          <- log( GDP_per_capita/1000)    # in thousands USD
log.labor        <- log( Labour_force_agr/1000)  # in million persons
log.temp         <- log( avg_temp)
log.precip       <- log( avg_precip)
water.stress     <- water
log.water.stress <- log( water.stress/100)
log.GDP.total    <- log( GDP_per_capita/1000 * Population_total/1000 )

## STAGE-1: MODEL WATER STRESS EVOLUTION ======================================
years <- seq( 1987, 2100, by=1 )
yd      <- years-1986 # normalized year dummy variable
ii      <- 5:32   # set the train data
ii50    <- 33:64  # set the proj data up to 2050
ii50_1  <- 1:32

water.EGY <- lm( log(water.stress[ii]) ~ yd[ii]  + log.land.std[ii] + 
		            log(avg_precip[ii])  + log(avg_temp[ii]) )
summary.lm(water.EGY)

alpha.EGY <- water.EGY$coef
names(alpha.EGY) <- c('Intercept', 'Year.dummy', 'Cropland', 'Avg.Precip',
				'Avg.Temp')

plot( year[id], water.stress[id], main="Observed data and model")
lines( year[ii], exp(water.EGY$fitted.values), col=2, lty=2)
#=============================================================================



## STAGE-2: MODEL DOMESTIC PRODUCTION QUANTITY ================================


## a. Domestic production for crops --------------------------------------------
ii <- 14:32
domestic.prod.EGY <- lm( log.crops.prod[ii] ~ yd[ii] + log.land.std[ii] + log.labor[ii]
				+ log.water.stress[ii] + log.temp[ii] + log.precip[ii] )
summary(domestic.prod.EGY)

DMat <- cbind(yd[ii], log.land.std[ii], log.labor[ii], log.water.stress[ii], log.temp[ii], log.precip[ii])
cor(DMat)

beta.crops.EGY <- domestic.prod.EGY$coef
names(beta.crops.EGY) <- c('Intercept', 'Year.dummy', 'Land', 'Labour', 
				'WaterStress', 'Avg.Temp', 'Avg.Precip')
beta.crops.EGY

plot( year[ii], log.crops.prod[ii], type="l", main="Domestic Production")
lines( year[ii], domestic.prod.EGY$fitted.values, col=2, lty=2)



## b. Domestic production for livestock ----------------------------------------
ii <- 14:32
domestic.prod.EGY <- lm( log.lvstock.prod[ii] ~ yd[ii] 
                         + log.land.std[ii] 
                         + log.labor[ii]
                         + log.water.stress[ii] 
                         + log.temp[ii]  )
summary(domestic.prod.EGY)


beta.lvstock.EGY <- domestic.prod.EGY$coef
names(beta.lvstock.EGY) <- c('Intercept', 'Year.dummy', 'Land', 'Labour', 
                           'WaterStress', 'Avg.Temp')
beta.lvstock.EGY

plot( year[ii], log.lvstock.prod[ii], type="l", main="Domestic Production")
lines( year[ii], domestic.prod.EGY$fitted.values, col=2, lty=2)


## c. Domestic production for fruits and vegetables ----------------------------
ii <- 14:32
domestic.prod.EGY <- lm( log.veget.prod[ii] ~ yd[ii] 
                         + log.land.std[ii] 
                         + log.labor[ii]
                         + log.water.stress[ii] 
                         + log.temp[ii])
summary(domestic.prod.EGY)


beta.veget.EGY <- domestic.prod.EGY$coef
names(beta.veget.EGY) <- c('Intercept', 'Year.dummy', 'Land', 'Labour', 
                             'WaterStress', 'Avg.Temp')
beta.veget.EGY

plot( year[ii], log.veget.prod[ii], type="l", main="Domestic Production")
lines( year[ii], domestic.prod.EGY$fitted.values, col=2, lty=2)


#===============================================================================



## STAGE-3: MODEL GROSS PRODUCT VALUE ==========================================

## a. Crops --------------------------------------------------------------------
gpv.crops.EGY <- lm( log.crops.gpv[ii] ~  log.crops.prod[ii] + log.GDP.total[ii]  )
summary.lm(gpv.crops.EGY)

gamma.crops.EGY <- gpv.crops.EGY$coef
names(gamma.crops.EGY) <- c('Intercept', 'Domestic.Prod', 'GDP')
gamma.crops.EGY

plot( year[ii], log.crops.gpv[ii], type="l", main="GPV from crops")
lines( year[ii], gpv.crops.EGY$fitted.values, col=2, lty=2)



## b. Livestock ----------------------------------------------------------------
gpv.lvstock.EGY <- lm( log.lvstock.gpv[ii] ~  log.lvstock.prod[ii] + log.GDP.total[ii]  )
summary.lm(gpv.lvstock.EGY)

gamma.lvstock.EGY <- gpv.lvstock.EGY$coef
names(gamma.lvstock.EGY) <- c('Intercept', 'Domestic.Prod', 'GDP')
gamma.lvstock.EGY

plot( year[ii], log.lvstock.gpv[ii], type="l", main="GPV from livestock")
lines( year[ii], gpv.lvstock.EGY$fitted.values, col=2, lty=2)



## c. Fruits and vegetables ----------------------------------------------------
gpv.veget.EGY <- lm( log.veget.gpv[ii] ~  log.veget.prod[ii] + log.GDP.total[ii]  )
summary.lm(gpv.veget.EGY)

gamma.veget.EGY <- gpv.veget.EGY$coef
names(gamma.veget.EGY) <- c('Intercept', 'Domestic.Prod', 'GDP')
gamma.veget.EGY

plot( year[ii], log.veget.gpv[ii], type="l", main="GPV from vegetables and fruits")
lines( year[ii], gpv.veget.EGY$fitted.values, col=2, lty=2)



#=============================================================================

## ===========================================================================
## Save Cobb-Douglas models' coefficients ------------------------------------

a <- alpha.EGY             # water stress model coefficients
b.c <- beta.crops.EGY      # crop production coefficients
b.l <- beta.lvstock.EGY    # livestock production coefficients
b.v <- beta.veget.EGY      # fruits and vegetables production coefficients
g.c <- gamma.crops.EGY     # GPV from crops model coef
g.l <- gamma.lvstock.EGY   # GPV from livestock model coef
g.v <- gamma.veget.EGY     # GPV from vegetables model coef

# scaling factor for labour force occupied in agriculture
pop_perc_agr <- 0.069

tpred    <- ii50
tpred_1  <- ii50_1
tpred_11 <- 1:7

# LOAD CLIMATE AND ECONOMY DATA ==============================================
file.name <- paste0(wd,"economy_scenarios_EGY_ETH.csv")
economy <- read.csv(file.name)
economy <- as.data.frame(economy)
economy.EGY <- economy[economy$Country=='EGY',]

file.name <- paste0(wd,"climate_scenarios_EGY_ETH.csv")
climate <- read.csv(file.name)
climate <- as.data.frame(climate)
climate.EGY <- climate[climate$Country=='EGY',]
#=============================================================================


### LAND USE PROJECTION ======================================================
# determine the land use scenario (use last 10 years to estimate trend)
yobs    <- cropland.ratio[23:32]
Xmat    <- cbind( rep(1,10), yd[23:32] )
b.land  <- as.numeric( solve( t(Xmat)%*%Xmat ) %*% t(Xmat) %*% yobs )

land.ratio.scen <- b.land[1] + b.land[2]*yd[tpred]
land.ratio.scen <- land.ratio.scen*(land.ratio.scen<=1 & land.ratio.scen >=0) + 
				(land.ratio.scen > 1)
#=============================================================================




### RUN THE 6 SCENARIOS ...




############################
##                        ##
##  SCENARIO 1: SSP1-1.9  ##
##                        ##
############################

## Call climate scenarios ----------------------------------------------------
prcp.scen    <- climate.EGY$precip_ssp1_1.9[tpred_1]
temp.scen    <- climate.EGY$temp_ssp1_1.9[tpred_1]
#-----------------------------------------------------------------------------

## Water stress prediction under SCENARIO 1 ----------------------------------
wpred_scen <- exp( a[1] + a[2]*tpred + a[3]*log(land.ratio.scen) + 
			a[4]*log(prcp.scen) + a[5]*log(temp.scen) )
#-----------------------------------------------------------------------------

## Population and Labour Force determination ---------------------------------
file <- paste0( wd, "ssp1_totpop_country", "818", ".rda" )
load(file)
ssp1.totp <- totp

T <- 2050
year1 <- seq(2020,T,by=5)
year2 <- seq(2019,T,by=1)
n <- dim(ssp1.totp)[2]
nyy <- length(year2)

# pre-allocate matrices ====================================================
PQ.crop    <- array(dim=c(nyy,n))
PQ.lvstock <- array(dim=c(nyy,n))
PQ.veget   <- array(dim=c(nyy,n))
GPV.crops   <- array(dim=c(nyy,n))
GPV.lvstock <- array(dim=c(nyy,n))
GPV.veget   <- array(dim=c(nyy,n))

for (j in 1:n) {
	xi <- year1
	yi <- ssp1.totp[tpred_11,j]
	spl <- spline(xi,yi,xout=year2)
	
	# Total population trajectory ------------------------------------------------
	pop.traj <- spl$y/1000

	# Agricultural labour force estimate -----------------------------------------
	log.labour.traj <- log( pop.traj * pop_perc_agr )

	# PRODUCTION QUANTITY PREDICTION STEP ========================================
	# a. Estimate Domestic Production Quantity for crops -------------------------
	PQ.crop[,j] <- exp( b.c[1] + b.c[2]*yd[ii50] + b.c[3]*log(land.ratio.scen) 
	                    + b.c[4]*log.labour.traj[ tpred_1 ] 
	                    + b.c[5]*log(wpred_scen/100)  
	                    + b.c[6]*log(temp.scen) + b.c[7]*log(prcp.scen) )
	
	# b. Estimate Domestic Production Quantity for livestock ---------------------
	PQ.lvstock[,j] <- exp( b.l[1] + b.l[2]*yd[ii50] + b.l[3]*log(land.ratio.scen) 
	                    + b.l[4]*log.labour.traj[ tpred_1 ] 
	                    + b.l[5]*log(wpred_scen/100) + b.l[6]*log(temp.scen) )
	
	# c. Estimate Domestic Production Quantity for vegetables --------------------
	PQ.veget[,j] <- exp( b.v[1] + b.v[2]*yd[ii50] + b.v[3]*log(land.ratio.scen) 
	                       + b.v[4]*log.labour.traj[ tpred_1 ] 
	                       + b.v[5]*log(wpred_scen/100) 
	                       + b.v[6]*log(temp.scen)  )
	#=============================================================================
	
	# GPV PREDICTION STEP ========================================================
	gdp.scen <- (economy.EGY$gdp_ssp1[tpred_1]/1000) * pop.traj
	
	# a. Predict GPV for crops ---------------------------------------------------
	GPV.crops[,j] <- exp( g.c[1] + g.c[2]*log(PQ.crop[,j]) + 
	                        g.c[3]*log(gdp.scen[tpred_1]) )
	
	# b. Predict GPV for livestock -----------------------------------------------
	GPV.lvstock[,j] <- exp( g.l[1] + g.l[2]*log(PQ.lvstock[,j]) + 
	                          g.l[3]*log(gdp.scen[tpred_1]) )
	
	# c. Predict GPV for vegetables ----------------------------------------------
	GPV.veget[,j] <- exp( g.v[1] + g.v[2]*log(PQ.veget[,j]) + 
	                        g.v[3]*log(gdp.scen[tpred_1]) )
}

# save the median projections
gpv.crops.scen_1   <- apply(GPV.crops, 1, median)
gpv.lvstock.scen_1 <- apply(GPV.lvstock, 1, median)
gpv.veget.scen_1   <- apply(GPV.veget, 1, median)



## Check projections compatibility ---------------------------------------------
par(mfrow=c(1,3))

# test crop proj
plot( year2, gpv.crops.scen_1, type='l', xlim=c(1987,2050), ylim=c(8,30) )
lines(seq(1987,2018,by=1), gpv_crops/1e+06, col=2)

# test lvstock proj
plot( year2, gpv.lvstock.scen_1, type='l', xlim=c(1987,2050), ylim=c(8,30) )
lines(seq(1987,2018,by=1), gpv_lvstock/1e+06, col=2)

# test veget proj
plot( year2, gpv.veget.scen_1, type='l', xlim=c(1987,2050), ylim=c(1,20) )
lines(seq(1987,2018,by=1), gpv_veg/1e+06, col=2)
#-------------------------------------------------------------------------------




############################
##                        ##
##  SCENARIO 2: SSP1-2.6  ##
##                        ##
############################

## Call climate scenarios ----------------------------------------------------
prcp.scen    <- climate.EGY$precip_ssp1_2.6[tpred_1]
temp.scen    <- climate.EGY$temp_ssp1_2.6[tpred_1]
#-----------------------------------------------------------------------------

## Water stress prediction under SCENARIO 2 ----------------------------------
wpred_scen <- exp( a[1] + a[2]*tpred + a[3]*log(land.ratio.scen) + 
                       a[4]*log(prcp.scen) + a[5]*log(temp.scen) )
#-----------------------------------------------------------------------------

## Population and Labour Force determination ---------------------------------
file <- paste0( wd, "ssp1_totpop_country", "818", ".rda" )
load(file)
ssp1.totp <- totp
n <- dim(ssp1.totp)[2]

# pre-allocate matrices ====================================================
PQ.crop    <- array(dim=c(nyy,n))
PQ.lvstock <- array(dim=c(nyy,n))
PQ.veget   <- array(dim=c(nyy,n))
GPV.crops   <- array(dim=c(nyy,n))
GPV.lvstock <- array(dim=c(nyy,n))
GPV.veget   <- array(dim=c(nyy,n))

for (j in 1:n) {
  xi <- year1
  yi <- ssp1.totp[tpred_11,j]
  spl <- spline(xi,yi,xout=year2)
  
  # Total population trajectory ---------------------------------------
  pop.traj <- spl$y/1000
  
  # Agricultural labour force estimate -------------------------------
  log.labour.traj <- log( pop.traj * pop_perc_agr )
  
  # PRODUCTION QUANTITY PREDICTION STEP ========================================
  # a. Estimate Domestic Production Quantity for crops -------------------------
  PQ.crop[,j] <- exp( b.c[1] + b.c[2]*yd[ii50] + b.c[3]*log(land.ratio.scen) 
                      + b.c[4]*log.labour.traj[ tpred_1 ] 
                      + b.c[5]*log(wpred_scen/100)  
                      + b.c[6]*log(temp.scen) + b.c[7]*log(prcp.scen) )
  
  # b. Estimate Domestic Production Quantity for livestock ---------------------
  PQ.lvstock[,j] <- exp( b.l[1] + b.l[2]*yd[ii50] + b.l[3]*log(land.ratio.scen) 
                         + b.l[4]*log.labour.traj[ tpred_1 ] 
                         + b.l[5]*log(wpred_scen/100) + b.l[6]*log(temp.scen) )
  
  # c. Estimate Domestic Production Quantity for vegetables --------------------
  PQ.veget[,j] <- exp( b.v[1] + b.v[2]*yd[ii50] + b.v[3]*log(land.ratio.scen) 
                       + b.v[4]*log.labour.traj[ tpred_1 ] 
                       + b.v[5]*log(wpred_scen/100) 
                       + b.v[6]*log(temp.scen)  )
  #=============================================================================
  
  # GPV PREDICTION STEP ========================================================
  gdp.scen <- (economy.EGY$gdp_ssp1[tpred_1]/1000) * pop.traj
  
  # a. Predict GPV for crops ---------------------------------------------------
  GPV.crops[,j] <- exp( g.c[1] + g.c[2]*log(PQ.crop[,j]) + 
                          g.c[3]*log(gdp.scen[tpred_1]) )
  
  # b. Predict GPV for livestock -----------------------------------------------
  GPV.lvstock[,j] <- exp( g.l[1] + g.l[2]*log(PQ.lvstock[,j]) + 
                            g.l[3]*log(gdp.scen[tpred_1]) )
  
  # c. Predict GPV for vegetables ----------------------------------------------
  GPV.veget[,j] <- exp( g.v[1] + g.v[2]*log(PQ.veget[,j]) + 
                          g.v[3]*log(gdp.scen[tpred_1]) )
}

# save the median projections --------------------------------------------------
gpv.crops.scen_2   <- apply(GPV.crops, 1, median)
gpv.lvstock.scen_2 <- apply(GPV.lvstock, 1, median)
gpv.veget.scen_2   <- apply(GPV.veget, 1, median)





############################
##                        ##
##  SCENARIO 3: SSP2-4.5  ##
##                        ##
############################

## Call climate scenarios ----------------------------------------------------
prcp.scen   <- climate.EGY$precip_ssp2_4.5[tpred_1]
temp.scen   <- climate.EGY$temp_ssp2_4.5[tpred_1]
#-----------------------------------------------------------------------------

## Water stress prediction under SCENARIO 3 ----------------------------------
wpred_scen <- exp( a[1] + a[2]*tpred + a[3]*log(land.ratio.scen) + 
                     a[4]*log(prcp.scen) + a[5]*log(temp.scen) )
#-----------------------------------------------------------------------------

## Population and Labour Force determination ---------------------------------
file <- paste0( wd, "ssp2_totpop_country", "818", ".rda" )
load(file)
ssp2.totp <- totp
n <- dim(ssp2.totp)[2]

# pre-allocate matrices ====================================================
PQ.crop    <- array(dim=c(nyy,n))
PQ.lvstock <- array(dim=c(nyy,n))
PQ.veget   <- array(dim=c(nyy,n))
GPV.crops   <- array(dim=c(nyy,n))
GPV.lvstock <- array(dim=c(nyy,n))
GPV.veget   <- array(dim=c(nyy,n))

for (j in 1:n) {
  xi <- year1
  yi <- ssp2.totp[tpred_11,j]
  spl <- spline(xi,yi,xout=year2)
  
  # Total population trajectory ---------------------------------------
  pop.traj <- spl$y/1000
  
  # Agricultural labour force estimate -------------------------------
  log.labour.traj <- log( pop.traj * pop_perc_agr )
  
  # PRODUCTION QUANTITY PREDICTION STEP ========================================
  # a. Estimate Domestic Production Quantity for crops -------------------------
  PQ.crop[,j] <- exp( b.c[1] + b.c[2]*yd[ii50] + b.c[3]*log(land.ratio.scen) 
                      + b.c[4]*log.labour.traj[ tpred_1 ] 
                      + b.c[5]*log(wpred_scen/100)  
                      + b.c[6]*log(temp.scen) + b.c[7]*log(prcp.scen) )
  
  # b. Estimate Domestic Production Quantity for livestock ---------------------
  PQ.lvstock[,j] <- exp( b.l[1] + b.l[2]*yd[ii50] + b.l[3]*log(land.ratio.scen) 
                         + b.l[4]*log.labour.traj[ tpred_1 ] 
                         + b.l[5]*log(wpred_scen/100) + b.l[6]*log(temp.scen) )
  
  # c. Estimate Domestic Production Quantity for vegetables --------------------
  PQ.veget[,j] <- exp( b.v[1] + b.v[2]*yd[ii50] + b.v[3]*log(land.ratio.scen) 
                       + b.v[4]*log.labour.traj[ tpred_1 ] 
                       + b.v[5]*log(wpred_scen/100) 
                       + b.v[6]*log(temp.scen)  )
  #=============================================================================
  
  # GPV PREDICTION STEP ========================================================
  gdp.scen <- (economy.EGY$gdp_ssp2[tpred_1]/1000) * pop.traj
  
  # a. Predict GPV for crops ---------------------------------------------------
  GPV.crops[,j] <- exp( g.c[1] + g.c[2]*log(PQ.crop[,j]) + 
                          g.c[3]*log(gdp.scen[tpred_1]) )
  
  # b. Predict GPV for livestock -----------------------------------------------
  GPV.lvstock[,j] <- exp( g.l[1] + g.l[2]*log(PQ.lvstock[,j]) + 
                            g.l[3]*log(gdp.scen[tpred_1]) )
  
  # c. Predict GPV for vegetables ----------------------------------------------
  GPV.veget[,j] <- exp( g.v[1] + g.v[2]*log(PQ.veget[,j]) + 
                          g.v[3]*log(gdp.scen[tpred_1]) )
}

# save the median projections --------------------------------------------------
gpv.crops.scen_3   <- apply(GPV.crops, 1, median)
gpv.lvstock.scen_3 <- apply(GPV.lvstock, 1, median)
gpv.veget.scen_3   <- apply(GPV.veget, 1, median)





	

############################
##                        ##
##  SCENARIO 4: SSP3-7.0  ##
##                        ##
############################

## Call climate scenarios ----------------------------------------------------
prcp.scen    <- climate.EGY$precip_ssp3_7.0[tpred_1]
temp.scen    <- climate.EGY$temp_ssp3_7.0[tpred_1]
#-----------------------------------------------------------------------------

## Water stress prediction under SCENARIO 4 ----------------------------------
wpred_scen <- exp( a[1] + a[2]*tpred + a[3]*log(land.ratio.scen) + 
                       a[4]*log(prcp.scen) + a[5]*log(temp.scen) )
#-----------------------------------------------------------------------------

## Population and Labour Force determination ---------------------------------
file <- paste0( wd, "ssp3_totpop_country", "818", ".rda" )
load(file)
ssp3.totp <- totp
n <- dim(ssp3.totp)[2]

# pre-allocate matrices ====================================================
PQ.crop    <- array(dim=c(nyy,n))
PQ.lvstock <- array(dim=c(nyy,n))
PQ.veget   <- array(dim=c(nyy,n))
GPV.crops   <- array(dim=c(nyy,n))
GPV.lvstock <- array(dim=c(nyy,n))
GPV.veget   <- array(dim=c(nyy,n))

for (j in 1:n) {
  xi <- year1
  yi <- ssp3.totp[tpred_11,j]
  spl <- spline(xi,yi,xout=year2)
  
  # Total population trajectory ---------------------------------------
  pop.traj <- spl$y/1000
  
  # Agricultural labour force estimate -------------------------------
  log.labour.traj <- log( pop.traj * pop_perc_agr )
  
  # PRODUCTION QUANTITY PREDICTION STEP ========================================
  # a. Estimate Domestic Production Quantity for crops -------------------------
  PQ.crop[,j] <- exp( b.c[1] + b.c[2]*yd[ii50] + b.c[3]*log(land.ratio.scen) 
                      + b.c[4]*log.labour.traj[ tpred_1 ] 
                      + b.c[5]*log(wpred_scen/100)  
                      + b.c[6]*log(temp.scen) + b.c[7]*log(prcp.scen) )
  
  # b. Estimate Domestic Production Quantity for livestock ---------------------
  PQ.lvstock[,j] <- exp( b.l[1] + b.l[2]*yd[ii50] + b.l[3]*log(land.ratio.scen) 
                         + b.l[4]*log.labour.traj[ tpred_1 ] 
                         + b.l[5]*log(wpred_scen/100) + b.l[6]*log(temp.scen) )
  
  # c. Estimate Domestic Production Quantity for vegetables --------------------
  PQ.veget[,j] <- exp( b.v[1] + b.v[2]*yd[ii50] + b.v[3]*log(land.ratio.scen) 
                       + b.v[4]*log.labour.traj[ tpred_1 ] 
                       + b.v[5]*log(wpred_scen/100) 
                       + b.v[6]*log(temp.scen)  )
  #=============================================================================
  
  # GPV PREDICTION STEP ========================================================
  gdp.scen <- (economy.EGY$gdp_ssp3[tpred_1]/1000) * pop.traj
  
  # a. Predict GPV for crops ---------------------------------------------------
  GPV.crops[,j] <- exp( g.c[1] + g.c[2]*log(PQ.crop[,j]) + 
                          g.c[3]*log(gdp.scen[tpred_1]) )
  
  # b. Predict GPV for livestock -----------------------------------------------
  GPV.lvstock[,j] <- exp( g.l[1] + g.l[2]*log(PQ.lvstock[,j]) + 
                            g.l[3]*log(gdp.scen[tpred_1]) )
  
  # c. Predict GPV for vegetables ----------------------------------------------
  GPV.veget[,j] <- exp( g.v[1] + g.v[2]*log(PQ.veget[,j]) + 
                          g.v[3]*log(gdp.scen[tpred_1]) )
}

# save the median projections --------------------------------------------------
gpv.crops.scen_4   <- apply(GPV.crops, 1, median)
gpv.lvstock.scen_4 <- apply(GPV.lvstock, 1, median)
gpv.veget.scen_4   <- apply(GPV.veget, 1, median)





############################
##                        ##
##  SCENARIO 5: SSP4-6.0  ##
##                        ##
############################

## Call climate scenarios ----------------------------------------------------
prcp.scen    <- climate.EGY$precip_rcp_60[tpred_1]
temp.scen    <- climate.EGY$temp_rcp_60[tpred_1]
#-----------------------------------------------------------------------------

## Water stress prediction under SCENARIO 1 ----------------------------------
wpred_scen <- exp( a[1] + a[2]*tpred + a[3]*log(land.ratio.scen) + 
                       a[4]*log(prcp.scen) + a[5]*log(temp.scen) )
#-----------------------------------------------------------------------------

## Population and Labour Force determination ---------------------------------
file <- paste0( wd, "ssp4_totpop_country", "818", ".rda" )
load(file)
ssp4.totp <- totp
n <- dim(ssp4.totp)[2]

# pre-allocate matrices ====================================================
PQ.crop    <- array(dim=c(nyy,n))
PQ.lvstock <- array(dim=c(nyy,n))
PQ.veget   <- array(dim=c(nyy,n))
GPV.crops   <- array(dim=c(nyy,n))
GPV.lvstock <- array(dim=c(nyy,n))
GPV.veget   <- array(dim=c(nyy,n))

for (j in 1:n) {
  xi <- year1
  yi <- ssp4.totp[tpred_11,j]
  spl <- spline(xi,yi,xout=year2)
  
  # Total population trajectory ---------------------------------------
  pop.traj <- spl$y/1000
  
  # Agricultural labour force estimate -------------------------------
  log.labour.traj <- log( pop.traj * pop_perc_agr )
  
  # PRODUCTION QUANTITY PREDICTION STEP ========================================
  # a. Estimate Domestic Production Quantity for crops -------------------------
  PQ.crop[,j] <- exp( b.c[1] + b.c[2]*yd[ii50] + b.c[3]*log(land.ratio.scen) 
                      + b.c[4]*log.labour.traj[ tpred_1 ] 
                      + b.c[5]*log(wpred_scen/100)  
                      + b.c[6]*log(temp.scen) + b.c[7]*log(prcp.scen) )
  
  # b. Estimate Domestic Production Quantity for livestock ---------------------
  PQ.lvstock[,j] <- exp( b.l[1] + b.l[2]*yd[ii50] + b.l[3]*log(land.ratio.scen) 
                         + b.l[4]*log.labour.traj[ tpred_1 ] 
                         + b.l[5]*log(wpred_scen/100) + b.l[6]*log(temp.scen) )
  
  # c. Estimate Domestic Production Quantity for vegetables --------------------
  PQ.veget[,j] <- exp( b.v[1] + b.v[2]*yd[ii50] + b.v[3]*log(land.ratio.scen) 
                       + b.v[4]*log.labour.traj[ tpred_1 ] 
                       + b.v[5]*log(wpred_scen/100) 
                       + b.v[6]*log(temp.scen)  )
  #=============================================================================
  
  # GPV PREDICTION STEP ========================================================
  gdp.scen <- (economy.EGY$gdp_ssp4[tpred_1]/1000) * pop.traj
  
  # a. Predict GPV for crops ---------------------------------------------------
  GPV.crops[,j] <- exp( g.c[1] + g.c[2]*log(PQ.crop[,j]) + 
                          g.c[3]*log(gdp.scen[tpred_1]) )
  
  # b. Predict GPV for livestock -----------------------------------------------
  GPV.lvstock[,j] <- exp( g.l[1] + g.l[2]*log(PQ.lvstock[,j]) + 
                            g.l[3]*log(gdp.scen[tpred_1]) )
  
  # c. Predict GPV for vegetables ----------------------------------------------
  GPV.veget[,j] <- exp( g.v[1] + g.v[2]*log(PQ.veget[,j]) + 
                          g.v[3]*log(gdp.scen[tpred_1]) )
}

# save the median projections --------------------------------------------------
gpv.crops.scen_5   <- apply(GPV.crops, 1, median)
gpv.lvstock.scen_5 <- apply(GPV.lvstock, 1, median)
gpv.veget.scen_5   <- apply(GPV.veget, 1, median)






############################
##                        ##
##  SCENARIO 6: SSP5-8.5  ##
##                        ##
############################

## Call climate scenarios ----------------------------------------------------
prcp.scen    <- climate.EGY$precip_ssp5_8.5[tpred_1]
temp.scen    <- climate.EGY$temp_ssp5_8.5[tpred_1]
#-----------------------------------------------------------------------------

## Water stress prediction under SCENARIO 1 ----------------------------------
wpred_scen <- exp( a[1] + a[2]*tpred + a[3]*log(land.ratio.scen) + 
                       a[4]*log(prcp.scen) + a[5]*log(temp.scen) )
#-----------------------------------------------------------------------------

## Population and Labour Force determination ---------------------------------
file <- paste0( wd, "ssp5_totpop_country", "818", ".rda" )
load(file)
ssp5.totp <- totp
n <- dim(ssp5.totp)[2]

# pre-allocate matrices ====================================================
PQ.crop    <- array(dim=c(nyy,n))
PQ.lvstock <- array(dim=c(nyy,n))
PQ.veget   <- array(dim=c(nyy,n))
GPV.crops   <- array(dim=c(nyy,n))
GPV.lvstock <- array(dim=c(nyy,n))
GPV.veget   <- array(dim=c(nyy,n))

for (j in 1:n) {
  xi <- year1
  yi <- ssp5.totp[tpred_11,j]
  spl <- spline(xi,yi,xout=year2)
  
  # Total population trajectory ---------------------------------------
  pop.traj <- spl$y/1000
  
  # Agricultural labour force estimate -------------------------------
  log.labour.traj <- log( pop.traj * pop_perc_agr )
  
  # PRODUCTION QUANTITY PREDICTION STEP ========================================
  # a. Estimate Domestic Production Quantity for crops -------------------------
  PQ.crop[,j] <- exp( b.c[1] + b.c[2]*yd[ii50] + b.c[3]*log(land.ratio.scen) 
                      + b.c[4]*log.labour.traj[ tpred_1 ] 
                      + b.c[5]*log(wpred_scen/100)  
                      + b.c[6]*log(temp.scen) + b.c[7]*log(prcp.scen) )
  
  # b. Estimate Domestic Production Quantity for livestock ---------------------
  PQ.lvstock[,j] <- exp( b.l[1] + b.l[2]*yd[ii50] + b.l[3]*log(land.ratio.scen) 
                         + b.l[4]*log.labour.traj[ tpred_1 ] 
                         + b.l[5]*log(wpred_scen/100) + b.l[6]*log(temp.scen) )
  
  # c. Estimate Domestic Production Quantity for vegetables --------------------
  PQ.veget[,j] <- exp( b.v[1] + b.v[2]*yd[ii50] + b.v[3]*log(land.ratio.scen) 
                       + b.v[4]*log.labour.traj[ tpred_1 ] 
                       + b.v[5]*log(wpred_scen/100) 
                       + b.v[6]*log(temp.scen)  )
  #=============================================================================
  
  # GPV PREDICTION STEP ========================================================
  gdp.scen <- (economy.EGY$gdp_ssp5[tpred_1]/1000) * pop.traj
  
  # a. Predict GPV for crops ---------------------------------------------------
  GPV.crops[,j] <- exp( g.c[1] + g.c[2]*log(PQ.crop[,j]) + 
                          g.c[3]*log(gdp.scen[tpred_1]) )
  
  # b. Predict GPV for livestock -----------------------------------------------
  GPV.lvstock[,j] <- exp( g.l[1] + g.l[2]*log(PQ.lvstock[,j]) + 
                            g.l[3]*log(gdp.scen[tpred_1]) )
  
  # c. Predict GPV for vegetables ----------------------------------------------
  GPV.veget[,j] <- exp( g.v[1] + g.v[2]*log(PQ.veget[,j]) + 
                          g.v[3]*log(gdp.scen[tpred_1]) )
}

# save the median projections --------------------------------------------------
gpv.crops.scen_6   <- apply(GPV.crops, 1, median)
gpv.lvstock.scen_6 <- apply(GPV.lvstock, 1, median)
gpv.veget.scen_6   <- apply(GPV.veget, 1, median)




##
## Store the median estimates to matrices
##

library(xtable)

# Crop valuation ---------------------------------------------------------------
GPV.crops.mat <- usd.2015.to.2017 * cbind( gpv.crops.scen_1, gpv.crops.scen_2, gpv.crops.scen_3,
                        gpv.crops.scen_4, gpv.crops.scen_5, gpv.crops.scen_6)
colnames(GPV.crops.mat) <- c('SSP1-1.9', 'SSP1-2.6', 'SSP2-4.5', 'SSP3-7.0',
                             'SSP4-6.0', 'SSP5-8.5')
rownames(GPV.crops.mat) <- seq(2019,2050,by=1)
tab <- GPV.crops.mat[c(7,12,17,22,27,32),]
tab
xtable(tab, type = "latex", file = "gpv_crops_EGY.tex")


# Livestock valuation ----------------------------------------------------------
GPV.lvstock.mat <- usd.2015.to.2017 * cbind( gpv.lvstock.scen_1, gpv.lvstock.scen_2, 
                          gpv.lvstock.scen_3, gpv.lvstock.scen_4,
                          gpv.lvstock.scen_5, gpv.lvstock.scen_6)
colnames(GPV.lvstock.mat) <- c('SSP1-1.9', 'SSP1-2.6', 'SSP2-4.5', 'SSP3-7.0',
                             'SSP4-6.0', 'SSP5-8.5')
rownames(GPV.lvstock.mat) <- seq(2019,2050,by=1)
tab <- GPV.lvstock.mat[c(7,12,17,22,27,32),]
tab
xtable(tab, type = "latex", file = "gpv_lvstock_EGY.tex")



# Fruits and Vegetables valuation ----------------------------------------------
GPV.veget.mat <- usd.2015.to.2017 * cbind( gpv.veget.scen_1, gpv.veget.scen_2, gpv.veget.scen_3,
                        gpv.veget.scen_4, gpv.veget.scen_5, gpv.veget.scen_6)
colnames(GPV.veget.mat) <- c('SSP1-1.9', 'SSP1-2.6', 'SSP2-4.5', 'SSP3-7.0',
                             'SSP4-6.0', 'SSP5-8.5')
rownames(GPV.veget.mat) <- seq(2019,2050,by=1)
tab <- GPV.veget.mat[c(7,12,17,22,27,32),]
tab
xtable(tab, type = "latex", file = "gpv_veget_EGY.tex")


# save data
usd.2015.to.2017 <- 1.03
ES.EGY <- list()
ES.EGY$cropland    <- land.ratio.scen * max.cropland
ES.EGY$gpv.crops   <- GPV.crops.mat * usd.2015.to.2017
ES.EGY$gpv.lvstock <- GPV.lvstock.mat * usd.2015.to.2017
ES.EGY$gpv.veget   <- GPV.veget.mat * usd.2015.to.2017
save(ES.EGY, file = paste0(wd,'ES_EGY.RData') )


## Plot estimates for crops ----------------------------------------------------
pic.file.name <- paste0(wd,'gpv_crop_EGY.png')
#png(file=pic.file.name, width=600, height=350)
png(file=pic.file.name)
plot( year2, gpv.crops.scen_1, type="l", ylim=c(17, 28), xlab="Year", 
        ylab="Gross Product Value (in billion USD)",
        main="Projected economic value for crop production (EGY)" )
lines( year2, gpv.crops.scen_2, col=2, lty=2)
lines( year2, gpv.crops.scen_3, col=3, lty=2)
lines( year2, gpv.crops.scen_4, col=4, lty=2)
lines( year2, gpv.crops.scen_5, col=5, lty=2)
lines( year2, gpv.crops.scen_6, col=6, lty=2)
legend( 2018, 28, lty=c(1,2,2,2,2,2), col=c(1,2,3,4,5,6), 
        legend=c('SSP1-1.9', 'SSP1-2.6', 'SSP2-4.5', 'SSP3-7.0', 'SSP4-6.0',
                 'SSP5-8.5') )
dev.off()



## Plot estimates for livestock ------------------------------------------------
pic.file.name <- paste0(wd,'gpv_lvstock_EGY.png')
#png(file=pic.file.name, width=600, height=350)
png(file=pic.file.name)
plot( year2, gpv.lvstock.scen_1, type="l", ylim=c(14, 29), xlab="Year", 
      ylab="Gross Product Value (in billion USD)",
      main="Projected economic value for livestock products (EGY)" )
lines( year2, gpv.lvstock.scen_2, col=2, lty=2)
lines( year2, gpv.lvstock.scen_3, col=3, lty=2)
lines( year2, gpv.lvstock.scen_4, col=4, lty=2)
lines( year2, gpv.lvstock.scen_5, col=5, lty=2)
lines( year2, gpv.lvstock.scen_6, col=6, lty=2)
legend( 2018, 29, lty=c(1,2,2,2,2,2), col=c(1,2,3,4,5,6), 
        legend=c('SSP1-1.9', 'SSP1-2.6', 'SSP2-4.5', 'SSP3-7.0', 'SSP4-6.0',
                 'SSP5-8.5') )
dev.off()


## Plot estimates for vegetables -----------------------------------------------
pic.file.name <- paste0(wd,'gpv_veget_EGY.png')
#png(file=pic.file.name, width=600, height=350)
png(file=pic.file.name)
plot( year2, gpv.veget.scen_1, type="l", ylim=c(8, 15), xlab="Year", 
      ylab="Gross Product Value (in billion USD)",
      main="Projected economic value for vegetable and fruits (EGY)" )
lines( year2, gpv.veget.scen_2, col=2, lty=2)
lines( year2, gpv.veget.scen_3, col=3, lty=2)
lines( year2, gpv.veget.scen_4, col=4, lty=2)
lines( year2, gpv.veget.scen_5, col=5, lty=2)
lines( year2, gpv.veget.scen_6, col=6, lty=2)
legend( 2018, 15, lty=c(1,2,2,2,2,2), col=c(1,2,3,4,5,6), 
        legend=c('SSP1-1.9', 'SSP1-2.6', 'SSP2-4.5', 'SSP3-7.0', 'SSP4-6.0',
                 'SSP5-8.5') )
dev.off()

