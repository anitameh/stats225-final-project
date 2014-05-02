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
rel_mobility = na.omit(alldata$relative)
abs_mobility = na.omit(alldata$absolute)
sample_size = as.numeric(alldata$sample.size)

## STEP 2: STANDARDIZE THE MOBILITY DATA

# first, demean
rel_mean = mean(na.omit(rel_mobility))
demean_rel_mobility = rel_mobility - rel_mean

abs_mean = mean(na.omit(abs_mobility))
demean_abs_mobility = abs_mobility - abs_mean

# now, multiply each mobility metric by the sqrt(sample size)
#     (Here, sample size = number of children in that county 
#     from the 1980-82 birth cohort.)
N = length(demean_rel_mobility)
rel_mobility_std = rep(0, N)
abs_mobility_std = rep(0, N)
for (i in seq(1,N)) {
  rel_mobility_std[i] = demean_rel_mobility[i]*sqrt(sample_size[i])
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
num_color = 10
plot_colors = brewer.pal(num_color,"YlOrRd")

class = classIntervals(rel_mobility, num_color, style="quantile")
colcode = findColours(class, plot_colors)

png( "relative_mobility.png", width = 5000, height = 4500 )
plot(USA_proj, col=colcode, border="grey", lwd=0.75)
legend(-78, 33, legend=names(attr(colcode, "table")), 
       fill=attr(colcode, "palette"), cex=5, bty="n")
dev.off()

# plot standardized relative mobility
class = classIntervals(rel_mobility_std, num_color, style="quantile")
colcode = findColours(class, plot_colors)

png( "relative_mobility_std.png", width = 5000, height = 4500 )
plot(USA_proj, col=colcode, border="grey", lwd=0.75)
legend(-78, 33, legend=names(attr(colcode, "table")), 
       fill=attr(colcode, "palette"), cex=5, bty="n")
dev.off()

# plot absolute mobility withOUT standardization, for comparison 
num_color = 10
plot_colors = rev(brewer.pal(num_color,"YlOrRd"))

class_abs = classIntervals(abs_mobility, num_color, style="quantile")
colcode_abs = findColours(class_abs, plot_colors)

png( "absolute_mobility.png", width = 5000, height = 4500 )
plot(USA_proj, col=colcode_abs, border="grey", lwd=0.75)
legend(-75, 35, legend=names(attr(colcode_abs, "table")), 
       fill=attr(colcode_abs, "palette"), cex=5, bty="n")
dev.off()

# plot standardized absolute mobility
class_abs = classIntervals(abs_mobility_std, num_color, style="quantile")
colcode_abs = findColours(class_abs, plot_colors)

png( "absolute_mobility_std.png", width = 5000, height = 4500 )
plot(USA_proj, col=colcode_abs, border="grey", lwd=0.75)
legend(-75, 35, legend=names(attr(colcode_abs, "table")), 
       fill=attr(colcode_abs, "palette"), cex=5, bty="n")
dev.off()

## STEP 4: CHECK FOR NONSTATIONARITY

# jitter coordinates
LONG = alldata$long
LAT = alldata$lat
coords = cbind(LONG, LAT)
jittered_coords = jitterDupCoords(coords, max=0.01)

# create geodata
rel_newdata = cbind(rel_mobility_std, jittered_coords)
my_rel_geodata = as.geodata(rel_newdata, coords.col = 2:3, data.col = 1)

abs_newdata = cbind(abs_mobility_std, jittered_coords)
my_abs_geodata = as.geodata(abs_newdata, coords.col = 2:3, data.col = 1)

# plot variog cloud using robust estimator
# Step 4a. relative mobility
my_rel_variog = variog(my_rel_geodata, option="cloud", estimator.type="modulus")
plot(my_rel_variog, 
     main = "Cressie estimator for Relative Mobility, Distance Unrestricted")

my_rel_variog = variog(my_rel_geodata, option="cloud", 
                       max.dist=20, estimator.type="modulus")
plot(my_rel_variog, 
     main = "Cressie estimator for Relative Mobility")

# Step 4b. absolute mobility
my_abs_variog = variog(my_abs_geodata, option="cloud", estimator.type="modulus")
plot(my_abs_variog, 
     main = "Cressie estimator for Absolute Mobility, Distance Unrestricted")

my_abs_variog = variog(my_abs_geodata, option="cloud", 
                       max.dist=20, estimator.type="modulus")
plot(my_abs_variog, main = "Cressie estimator for Absolute Mobility")


## STEP 5: FIT VARIOGRAM

# Step 5A. relative mobility 

# study binning
my_rel_variog = variog(my_rel_geodata, breaks=seq(0,50, length=20), estimator.type="modulus")
plot(my_rel_variog$uvec[1:length(my_rel_variog$uvec)], 
     cumsum(my_rel_variog$n)/sum(my_rel_variog$n), 
     main="Percent of Data Captured Within a Bin", xlab="Bins (degree)", ylab="Amount of Data (%)")

# variog with unrestricted data
my_rel_variog = variog(my_rel_geodata, estimator.type="modulus")
rel_dist = my_rel_variog$u
rel_semivar = my_rel_variog$v
plot(rel_dist, rel_semivar, 
     main="Relative Mobility Variogram Fit, Distance Unrestricted", xlab="distance", ylab="semivariance")

# now, fit several variograms
init_sill = 30
init_range = 60

print("Spherical")
spherical = variofit(my_rel_variog, c(init_sill, init_range), cov.model="spherical", weights="cressie")
lines(spherical, col="red")
print(spherical)

print("Squared Exponential")
sq_exp = variofit(my_rel_variog, c(init_sill, init_range), cov.model="gaussian", weights="cressie")
lines(sq_exp, col="green")
print(sq_exp)

print("Wave")
wave = variofit(my_rel_variog, c(init_sill, init_range), cov.model="wave", weights="cressie")
lines(wave, col="blue")
print(wave)

print("Exponential")
exp = variofit(my_rel_variog, c(init_sill, init_range), cov.model="exponential", weights="cressie")
lines(exp, col="orange")
print(exp)

print("Cubic")
cubic = variofit(my_rel_variog, c(init_sill, init_range), cov.model="cubic", weights="cressie")
lines(cubic, col="yellow")
print(cubic)

print("Circular")
circ = variofit(my_rel_variog, c(init_sill, init_range), cov.model="circular", weights="cressie")
lines(circ, col="black")
print(circ)

# rel variog with restricted data
my_rel_variog = variog(my_rel_geodata, max.dist=20, estimator.type="modulus")
rel_dist = my_rel_variog$u
rel_semivar = my_rel_variog$v
plot(rel_dist, rel_semivar, main="Relative Mobility Variogram Fit", xlab="distance", ylab="semivariance")

# now, fit several variograms
init_sill = 11
init_range = 20

print("Spherical")
spherical = variofit(my_rel_variog, c(init_sill, init_range), cov.model="spherical", weights="cressie")
lines(spherical, col="red")
print(spherical)

print("Squared Exponential")
sq_exp = variofit(my_rel_variog, c(init_sill, init_range), cov.model="gaussian", weights="cressie")
lines(sq_exp, col="green")
print(sq_exp)

print("Wave")
wave = variofit(my_rel_variog, c(init_sill, init_range), cov.model="wave", weights="cressie")
lines(wave, col="blue")
print(wave)

print("Exponential")
exp = variofit(my_rel_variog, c(init_sill, init_range), cov.model="exponential", weights="cressie")
lines(exp, col="orange")
print(exp)

print("Cubic")
cubic = variofit(my_rel_variog, c(init_sill, init_range), cov.model="cubic", weights="cressie")
lines(cubic, col="yellow")
print(cubic)

print("Circular")
circ = variofit(my_rel_variog, c(init_sill, init_range), cov.model="circular", weights="cressie")
lines(circ, col="black")
print(circ)

# Step 2B. absolute mobility

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
plot(abs_dist, abs_semivar, 
     main="Absolute Mobility Variogram Fit, Distance Unrestricted", 
     xlab="distance", ylab="semivariance")

# now, fit several variograms
init_sill = 70000
init_range = 60

print("Spherical")
spherical = variofit(my_abs_variog, c(init_sill, init_range), cov.model="spherical", weights="cressie")
lines(spherical, col="red")
print(spherical)

print("Squared Exponential")
sq_exp = variofit(my_abs_variog, c(init_sill, init_range), cov.model="gaussian", weights="cressie")
lines(sq_exp, col="green")
print(sq_exp)

print("Wave")
wave = variofit(my_abs_variog, c(init_sill, init_range), cov.model="wave", weights="cressie")
lines(wave, col="blue")
print(wave)

print("Exponential")
exp = variofit(my_abs_variog, c(init_sill, init_range), cov.model="exponential", weights="cressie")
lines(exp, col="orange")
print(exp)

print("Cubic")
cubic = variofit(my_abs_variog, c(init_sill, init_range), cov.model="cubic", weights="cressie")
lines(cubic, col="yellow")
print(cubic)

print("Circular")
circ = variofit(my_abs_variog, c(init_sill, init_range), cov.model="circular", weights="cressie")
lines(circ, col="black")
print(circ)

# variog with restricted data
my_abs_variog = variog(my_abs_geodata, max.dist=20, estimator.type="modulus")
abs_dist = my_abs_variog$u
abs_semivar = my_abs_variog$v
plot(abs_dist, abs_semivar, main="Absolute Mobility Variogram Fit", 
     xlab="distance", ylab="semivariance")

# now, fit several variograms
init_sill = 70500
init_range = 20

print("Spherical")
spherical = variofit(my_abs_variog, c(init_sill, init_range), cov.model="spherical", weights="cressie")
lines(spherical, col="red")
print(spherical)

print("Squared Exponential")
sq_exp = variofit(my_abs_variog, c(init_sill, init_range), cov.model="gaussian", weights="cressie")
lines(sq_exp, col="green")
print(sq_exp)

print("Wave")
wave = variofit(my_abs_variog, c(init_sill, init_range), cov.model="wave", weights="cressie")
lines(wave, col="blue")
print(wave)

print("Exponential")
exp = variofit(my_abs_variog, c(init_sill, init_range), cov.model="exponential", weights="cressie")
lines(exp, col="orange")
print(exp)

print("Cubic")
cubic = variofit(my_abs_variog, c(init_sill, init_range), cov.model="cubic", weights="cressie")
lines(cubic, col="yellow")
print(cubic)

print("Circular")
circ = variofit(my_abs_variog, c(init_sill, init_range), cov.model="circular", weights="cressie")
lines(circ, col="black")
print(circ)


## STEP 6: ENVELOPE PLOTTING

# Step 6a. relative mobility
env_spherical = variog.model.env(my_rel_geodata, 
                                 obj.var=my_rel_variog, model.pars=spherical, nsim=10)
plot(my_rel_variog, envelope = env_spherical, 
     main="Relative Mobility Envelope: Emp Variograms, Permutation ") 

# Step 6b. absolute mobility
env_circular = variog.model.env(my_abs_geodata, 
                                obj.var=my_abs_variog, model.pars=circ, nsim=10)
plot(my_abs_variog, envelope = env_circular, 
     main="Absolute Mobility Envelope: Emp Variograms, Permutation ") 

## STEP 7: FINISH KRIGING
pred_grid = expand.grid(seq(-130, -65, 1), seq(20, 60, 1))

# Step 7a. relative mobility
rel_tau2 = 4.2462
rel_sigma2 = 2986.3702
rel_phi = 14498.1044

rel_krige_control = krige.control(type.krige="ok", cov.model="spherical", 
                                  cov.pars=c(rel_sigma2, rel_phi), nugget=rel_tau2)

# predict and plot!
rel_krige_pred = krige.conv(my_rel_geodata, locations=pred_grid, krige=rel_krige_control)

image(rel_krige_pred, pred_grid, main="Relative Mobility Ordinary Kriging")
plot(USA_proj, border="dark grey", add=TRUE)
contour(rel_krige_pred, pred_grid, add=TRUE)
legend.krige(x.leg=c(-85, -70), y.leg=c(54, 58), rel_krige_pred$predict)

# Step 7a. absolute mobility
abs_tau2 = 15985.6409
abs_sigma2 = 55403.1106
abs_phi = 18.9535

abs_krige_control = krige.control(type.krige="ok", cov.model="circular", 
                                  cov.pars=c(abs_sigma2, abs_phi), nugget=abs_tau2)

# predict and plot!
abs_krige_pred = krige.conv(my_abs_geodata, locations=pred_grid, krige=abs_krige_control)

image(rel_krige_pred, pred_grid, main="Absolute Mobility Ordinary Kriging")
plot(USA_proj, border="dark grey", add=TRUE)
contour(abs_krige_pred, pred_grid, add=TRUE)
legend.krige(x.leg=c(-85, -70), y.leg=c(54, 58), abs_krige_pred$predict)
