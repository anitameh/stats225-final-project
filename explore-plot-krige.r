#############################
# @final-project: Part 1
# @course: Spatial Statistics
# @author: Anita Mehrotra
# @date: April 13, 2014
#############################

# load necessary packages
library("maptools")
library("spatial")
library("RColorBrewer")
library("geoR")
library("classInt")
library("ggplot2")
library("grDevices")

#setRepositories(ind=1:2) # installing with MacOS
#install.packages("rgdal")
library("rgdal")

# set working directory
wd = "/Users/anita/Documents/Spring 2014/Final Projects/Stats 225/"
setwd(wd)

## STEP 1: LOAD DATA & TRANSFORM

# pull in shapefile data of USA counties
shape = readShapePoly(paste(wd, "/data/gz_2010_us_050_00_500k/gz_2010_us_050_00_500k.shp", sep=""))

# remove Virgin Islands
counties_removeVI = c("St. Croix", "St. John", "St. Thomas")

# remove Hawaii
counties_removeHI = c("Hawaii", "Honolulu", "Kalawao", "Kauai", "Maui")

# remove Alaska
counties_removeAK = c("Aleutians East", "Aleutians West", "Anchorage", "Bethel", "Bristol Bay",
                      "Denali", "Dillingham", "Fairbanks North Star", "Haines", "Juneau",
                      "Kenai Peninsula", "Ketchikan Gateway", "Kodiak Island", "Matanuska-Susitna",
                      "Nome", "North Slope", "Northwest Arctic", "Lake and Peninsula",
                      "Haines", "Sitka", "Hoonah-Angoon", "Skagway", "Southeast Fairbanks", 
                      "Valdez-Cordova", "Wade Hampton", "Wrangell", "Petersburg", "Yakutat", 
                      "Yukon-Koyukuk", "Prince of Wales-Hyder")

# remove Puerto Rico
counties_removePR = c("Adjuntas", "Aguada", "Aguadilla", "Aguas Buenas", "Aibonito", "A\xf1asco",
                      "Arecibo", "Arroyo", "Barceloneta", "Barranquitas", "Bayam\xf3n", "Cabo Rojo",
                      "Caguas", "Camuy", "Can\xf3vanas", "Carolina", "Cata\xf1o", "Cayey", "Ceiba",
                      "Ciales", "Cidra", "Coamo", "Comer\xedo", "Corozal", "Culebra", "Dorado", "Fajardo",
                      "Florida", "Gu\xe1nica", "Guayama", "Guayanilla", "Guaynabo", "Gurabo", "Hatillo",
                      "Hormigueros", "Humacao", "Isabela", "Jayuya", "Juana D\xedaz", "Juncos", "Lajas",
                      "Lares", "Las Mar\xedas", "Las Piedras", "Lo\xedza", "Luquillo", "Manat\xed", "Maricao",
                      "Maunabo", "Mayag\xfcez", "Moca", "Morovis", "Naguabo", "Naranjito", "Orocovis",
                      "Patillas", "Pe\xf1uelas", "Ponce", "Quebradillas", "Rinc\xf3n", "R\xedo Grande", "Sabana Grande",
                      "Salinas", "San Germ\xe1n", "San Juan", "San Lorenzo", "San Sebasti\xe1n", "Santa Isabel",
                      "Toa Alta", "Toa Baja", "Trujillo Alto", "Utuado", "Vega Alta", "Vega Baja", "Vieques",
                      "Villalba", "Yabucoa", "Yauco")

counties_remove = c( counties_removeVI, counties_removeHI, counties_removeAK)
ind_to_remove = match(counties_remove, shape$NAME)

# handle Puerto Rico separately 
lenPR = length(counties_removePR)
PR_ind_to_remove = match(counties_removePR, shape$NAME)
PR = append(sort(PR_ind_to_remove)[2:lenPR], 3156)

all_ind_to_remove = c(ind_to_remove, PR)
shape_data = shape[-na.omit(all_ind_to_remove),]

# read in mobility data (data does NOT include AK and HI)
#data = read.csv(paste(wd, "/data/absolute-relative-mobility-county.csv", sep=""))

# read in long-lat data
alldata = read.csv(paste(wd, "data/complete_data.csv", sep=""))
vizdata = read.csv(paste(wd, "data/viz_all_data.csv", sep=""))
#rel_mobility = na.omit(alldata$relative)
#abs_mobility = na.omit(alldata$absolute)
abs_mobility = vizdata$absolute
sample_size = as.numeric(alldata$sample.size)

## STEP 2: STANDARDIZE THE MOBILITY DATA

# first, demean
#rel_mean = mean(na.omit(rel_mobility))
#demean_rel_mobility = rel_mobility - rel_mean

abs_mean = mean(na.omit(abs_mobility))
demean_abs_mobility = abs_mobility - abs_mean

# now, multiply each mobility metric by the sqrt(sample size)
#     (Here, sample size = number of children in that county 
#     from the 1980-82 birth cohort.)
N = length(abs_mobility)
#rel_mobility_std = rep(0, N)
abs_mobility_std = rep(0, N)
for (i in seq(1,N)) {
#  rel_mobility_std[i] = demean_rel_mobility[i]*sqrt(sample_size[i])
  abs_mobility_std[i] = demean_abs_mobility[i]*sqrt(sample_size[i])
}

## STEP 3: PLOTTING

# project long-lat data to x-y data
proj4string(shape_data) = CRS("+proj=longlat")
proj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
USA_proj = spTransform(shape_data, CRS(proj))

# plot data
# plot(USA_proj)

# plot relative mobility withOUT standardization, just for comparison
#num_color = 9
#plot_colors = brewer.pal(num_color,"YlOrRd")

#class = classIntervals(rel_mobility, num_color, style="quantile")
#colcode = findColours(class, plot_colors)

#png( "relative_mobility.png", width = 5000, height = 4500 )
#plot(USA_proj, col=colcode, border="grey", lwd=0.75)
#legend(-78, 33, legend=names(attr(colcode, "table")), 
#       fill=attr(colcode, "palette"), cex=5, bty="n")
#dev.off()

# plot standardized relative mobility
#class = classIntervals(rel_mobility_std, num_color, style="quantile")
#colcode = findColours(class, plot_colors)

#png( "relative_mobility_std.png", width = 5000, height = 4500 )
#plot(USA_proj, col=colcode, border="grey", lwd=0.75)
#legend(-78, 33, legend=names(attr(colcode, "table")), 
#       fill=attr(colcode, "palette"), cex=5, bty="n")
#dev.off()

# plot absolute mobility withOUT standardization, for comparison 
num_color = 9
plot_colors = rev(brewer.pal(num_color,"YlOrRd"))

class_abs = classIntervals(abs_mobility, num_color, style="quantile")
colcode_abs = findColours(class_abs, plot_colors)

png( "absolute_mobility.png", width = 5000, height = 4500 )
#png( "absolute_mobility_vizdata.png", width = 5000, height = 4500 )
plot(USA_proj, col=colcode_abs, border="grey", lwd=0.75)
legend(-75, 35, legend=names(attr(colcode_abs, "table")), 
       fill=attr(colcode_abs, "palette"), cex=5, bty="n")
dev.off()

# plot standardized absolute mobility
class_abs = classIntervals(abs_mobility_std, num_color, style="quantile")
colcode_abs = findColours(class_abs, plot_colors)

png( "absolute_mobility_std_vizdata.png", width = 5000, height = 4500 )
plot(USA_proj, col=colcode_abs, border="grey", lwd=0.75)
legend(-75, 35, legend=names(attr(colcode_abs, "table")), 
       fill=attr(colcode_abs, "palette"), cex=5, bty="n")
dev.off()

## STEP 4: CHECK FOR NONSTATIONARITY

# jitter coordinates
LONG = vizdata$long
LAT = vizdata$lat
coords = cbind(LONG, LAT)
jittered_coords = jitterDupCoords(coords, max=0.01)

# create geodata
abs_newdata = cbind(abs_mobility_std, jittered_coords)
my_abs_geodata = as.geodata(abs_newdata, coords.col = 2:3, data.col = 1)

# plot variog cloud using robust estimator
my_abs_variog = variog(my_abs_geodata, option="cloud", estimator.type="modulus")
png( "absolute_mobility_cloud_unrestricted.png", width = 5000, height = 4500 )
plot(my_abs_variog$u, my_abs_variog$v, pch=10,
     col=rgb(red=0, green=0, blue=255, alpha=50, max=255),
     xlab="Distance", ylab="Semivariance",
     main = "Cressie estimator, Distance Unrestricted")
dev.off()

my_abs_variog = variog(my_abs_geodata, option="cloud", 
                       max.dist=20, estimator.type="modulus")
png( "absolute_mobility_cloud_restricted.png", width = 5000, height = 4500 )
plot(my_abs_variog$u, my_abs_variog$v, pch=10,
     col=rgb(red=0, green=0, blue=255, alpha=50, max=255),
     xlab="Distance", ylab="Semivariance",
     main = "Cressie estimator, Max Dist = 20")
dev.off()

## STEP 5: FIT VARIOGRAM

# study binning
my_abs_variog = variog(my_abs_geodata, breaks=seq(0,50, length=20), estimator.type="modulus")
plot(my_abs_variog$uvec[1:length(my_abs_variog$uvec)], 
     cumsum(my_abs_variog$n)/sum(my_abs_variog$n), 
     main="Percent of Data Captured Within a Bin", 
     xlab="Bins (degree)", ylab="Amount of Data (%)")

# abs variog with unrestricted data
my_abs_variog = variog(my_abs_geodata, estimator.type="modulus")
abs_dist = my_abs_variog$u
abs_semivar = my_abs_variog$v
plot(abs_dist, abs_semivar, col=rgb(red=0, green=0, blue=255, alpha=100, max=255), 
     pch=16, cex=2.5,
     main="Empirical Variogram, Distance Unrestricted", 
     xlab="distance", ylab="semivariance")

# now, fit several variograms
init_sill = 30000
init_range = 100

print("Spherical")
spherical = variofit(my_abs_variog, c(init_sill, init_range), 
                     cov.model="spherical", weights="cressie")
lines(spherical, col="red", lwd=2)
print(spherical)

print("Squared Exponential")
sq_exp = variofit(my_abs_variog, c(init_sill, init_range), 
                  cov.model="gaussian", weights="cressie")
lines(sq_exp, col="green", lwd=2)
print(sq_exp)

print("Wave")
wave = variofit(my_abs_variog, c(init_sill, init_range), 
                cov.model="wave", weights="cressie")
lines(wave, col="black", lwd=2)
print(wave)

print("Exponential")
exp = variofit(my_abs_variog, c(init_sill, init_range), 
               cov.model="exponential", weights="cressie")
lines(exp, col="orange")
print(exp)

print("Cubic")
cubic = variofit(my_abs_variog, c(init_sill, init_range), 
                 cov.model="cubic", weights="cressie")
lines(cubic, col="yellow")
print(cubic)

print("Circular")
circ = variofit(my_abs_variog, c(init_sill, init_range), 
                cov.model="circular", weights="cressie")
lines(circ, col="purple")
print(circ)

legend(x=70, y=14000, fill=c("red", "green", "black", "orange", "yellow", "purple"), 
       legend=c("spherical", "sq exp", "wave", "exp", "cubic", "circular"))

# variog with restricted data
my_abs_variog = variog(my_abs_geodata, max.dist=20, estimator.type="modulus")
abs_dist = my_abs_variog$u
abs_semivar = my_abs_variog$v
plot(abs_dist, abs_semivar, col=rgb(red=0, green=0, blue=255, alpha=100, max=255), 
     pch=16, cex=2.5,
     main="Empirical Variogram, Max Dist = 20 deg", 
     xlab="distance", ylab="semivariance")

# now, fit several variograms
init_sill = 30000
init_range = 20

print("Spherical")
spherical = variofit(my_abs_variog, c(init_sill, init_range), 
                     cov.model="spherical", weights="cressie")
lines(spherical, col="red", lwd=2)
print(spherical)

print("Squared Exponential")
sq_exp = variofit(my_abs_variog, c(init_sill, init_range), 
                  cov.model="gaussian", weights="cressie")
lines(sq_exp, col="green", lwd=2)
print(sq_exp)

print("Wave")
wave = variofit(my_abs_variog, c(init_sill, init_range), 
                cov.model="wave", weights="cressie")
lines(wave, col="black", lwd=2)
print(wave)

print("Exponential")
exp = variofit(my_abs_variog, c(init_sill, init_range), 
               cov.model="exponential", weights="cressie")
lines(exp, col="orange", lwd=2)
print(exp)

print("Cubic")
cubic = variofit(my_abs_variog, c(init_sill, init_range), 
                 cov.model="cubic", weights="cressie")
lines(cubic, col="yellow", lwd=2)
print(cubic)

print("Circular")
circ = variofit(my_abs_variog, c(init_sill, init_range), 
                cov.model="circular", weights="cressie")
lines(circ, col="purple", lwd=2)
print(circ)

legend(x=15, y=18000, fill=c("red", "green", "black", "orange", "yellow", "purple"), 
       legend=c("spherical", "sq exp", "wave", "exp", "cubic", "circular"))


## STEP 6: ENVELOPE PLOTTING

env = variog.model.env(my_abs_geodata, obj.var=my_abs_variog, model.pars=spherical, nsim=10)
plot(my_abs_variog, envelope = env, 
     main="Envelope using Spherical Parameters") 

## STEP 7: FINISH KRIGING

pred_grid = expand.grid(seq(-127, -65, 1), seq(25, 50, 1))

abs_tau2 = 7455.1672
abs_sigma2 = 32794.5477
abs_phi = 32.2863

abs_krige_control = krige.control(type.krige="ok", cov.model="spherical", 
                                  cov.pars=c(abs_sigma2, abs_phi), nugget=abs_tau2)

# predict
abs_krige_pred = krige.conv(my_abs_geodata, locations=pred_grid, krige=abs_krige_control)

# plot
image(abs_krige_pred, pred_grid, col=heat.colors(200), 
      main="Absolute Mobility Ordinary Kriging")
plot(USA_proj, border=rgb(red=67,blue=70,green=75,max=225), add=TRUE)
contour(abs_krige_pred, pred_grid, col="white", add=TRUE)
legend.krige(x.leg=c(-93, -70), y.leg=c(52, 56), abs_krige_pred$predict)

# plot standard errors
image(abs_krige_pred, val=sqrt(abs_krige_pred$krige.var), col=heat.colors(200), 
      main="Standard Errors")
plot(USA_proj, border=rgb(red=67,blue=70,green=75,max=225), add=TRUE)
contour(abs_krige_pred, pred_grid, col="white", add=TRUE)
legend.krige(x.leg=c(-93, -70), y.leg=c(52, 56), sqrt(abs_krige_pred$krige.var))

# Step 8. remove state effect