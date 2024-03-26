##### Load required packages ######

require(ggplot2)
require(sf)
require(sp)
require(rgdal)
require(grid)
require(gridExtra)
require(egg)
require(rnaturalearth)
require(stars)
require(viridis)
#require(INLA)
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
require(inlabru)
require(R2jags) 
require(plotly)
require(dplyr)
require(tidyr)
require(AICcmodavg)
require(cowplot)

##### Read in data ######

# See metadata for variable descriptions

#look at herb leaf out time as well

DOY.df1 <- read.csv('npn_mw_inat_master.csv',
                   header = TRUE, sep = ",")

pheno1 = subset(DOY.df1, Latitude > 34)
#pheno_wood = subset(pheno, Species_Type == "Northern Forest Flower")
pheno1$Phenophase_Status = abs(pheno1$Phenophase_Status)
pheno1$Growth_Habit[pheno1$Growth_Habit == "Shrub/Tree"] <- "Tree"
p1 = subset(pheno1, Phenophase_Name == "Open flowers" & Growth_Habit != "Tree")
p2 = subset(pheno1, Phenophase_Name == "Breaking leaf buds" & Growth_Habit == "Tree")
p3 = rbind(p1,p2)

p4 = subset(p3, Phenophase_Status == 1)
p5 = subset(p4, Day_of_Year > 1)

DOY.df = subset(p5, Day_of_Year < 215)
DOY.df = DOY.df %>% drop_na(Tmean_spring)
DOY.df = DOY.df %>% drop_na(Elevation)
DOY.df = DOY.df %>% drop_na(Swe_spring)
DOY.df = DOY.df %>% drop_na(Tmean_winter)
DOY.df = DOY.df %>% drop_na(gdd_0_spring_accum)

DOY.df$Tmean_spring = scale(DOY.df$Tmean_spring)
DOY.df$Elevation = scale(DOY.df$Elevation)

# Split df by region and lifeform for JAGS models
df.NF.F <- DOY.df[DOY.df$Species_Type == "Northern Forest Flower" &
                    DOY.df$Growth_Habit != "Tree",]
df.NF.T <- DOY.df[DOY.df$Species_Type == "Northern Forest Flower" &
                    DOY.df$Growth_Habit == "Tree",]
df.NF.T <- within(df.NF.T, Scientific_Name[Common_Name == 'American beech'] <- 'Fagus grandifolia')
df.AL.F <- DOY.df[DOY.df$Species_Type == "Alpine Flower",]
df.AL.F <- within(df.AL.F, Scientific_Name[Common_Name == 'Alpine bilberry'] <- 'Vaccinium uliginosum')

#Species only df
#Alpine
df.AL.F1 = subset(df.AL.F, Scientific_Name == "Carex bigelowii")
df.AL.F2 = subset(df.AL.F, Scientific_Name == "Diapensia lapponica")
df.AL.F3 = subset(df.AL.F, Scientific_Name == "Geum peckii")
df.AL.F4 = subset(df.AL.F, Scientific_Name == "Rhododendron groenlandicum")
df.AL.F5 = subset(df.AL.F, Scientific_Name == "Vaccinium uliginosum")
df.AL.F6 = subset(df.AL.F, Scientific_Name == "Vaccinium vitis-idaea")

#Forbs
df.NF.F1 = subset(df.NF.F, Scientific_Name == "Clintonia borealis")
df.NF.F2 = subset(df.NF.F, Scientific_Name == "Cornus canadensis")
df.NF.F3 = subset(df.NF.F, Scientific_Name == "Erythronium americanum")
df.NF.F4 = subset(df.NF.F, Scientific_Name == "Maianthemum canadense")
df.NF.F5 = subset(df.NF.F, Scientific_Name == "Oxalis montana")
df.NF.F6 = subset(df.NF.F, Scientific_Name == "Sanguinaria canadensis")
df.NF.F7 = subset(df.NF.F, Scientific_Name == "Trientalis borealis")
df.NF.F8 = subset(df.NF.F, Scientific_Name == "Trillium erectum")
df.NF.F9 = subset(df.NF.F, Scientific_Name == "Trillium undulatum")
df.NF.F10 = subset(df.NF.F, Scientific_Name == "Viburnum lantanoides")

#Trees
df.NF.T1 = subset(df.NF.T, Scientific_Name == "Acer pensylvanicum")
df.NF.T2 = subset(df.NF.T, Scientific_Name == "Acer rubrum")
df.NF.T3 = subset(df.NF.T, Scientific_Name == "Acer saccharum")
df.NF.T4 = subset(df.NF.T, Scientific_Name == "Acer spicatum")
df.NF.T5 = subset(df.NF.T, Scientific_Name == "Betula alleghaniensis")
df.NF.T6 = subset(df.NF.T, Scientific_Name == "Betula lenta")
df.NF.T7 = subset(df.NF.T, Scientific_Name == "Betula papyrifera")
df.NF.T8 = subset(df.NF.T, Scientific_Name == "Fagus grandifolia")
df.NF.T9 = subset(df.NF.T, Scientific_Name == "Fraxinus americana")
df.NF.T10 = subset(df.NF.T, Scientific_Name == "Ostrya virginiana")
df.NF.T11 = subset(df.NF.T, Scientific_Name == "Prunus pensylvanica")
df.NF.T12 = subset(df.NF.T, Scientific_Name == "Prunus serotina")
df.NF.T13 = subset(df.NF.T, Scientific_Name == "Quercus rubra")
df.NF.T14 = subset(df.NF.T, Scientific_Name == "Sorbus americana")
df.NF.T15 = subset(df.NF.T, Scientific_Name == "Tilia americana")

#
sd(df.NF.F$Tmean_spring)
sd(df.NF.T$Tmean_spring)
sd(df.AL.F$Tmean_spring)

count(df.NF.F, vars = "Scientific_Name")
count(df.NF.T, vars = "Scientific_Name")
count(df.AL.F, vars = "Scientific_Name")

lmf = lm(Day_of_Year ~ Tmean_summer + Latitude + Elevation + Tmean_winter + Tmean_spring + Prcp_summer + Swe_spring + Vp_summer +
           gdd_0_summer_accum + Longitude, data = DOY.df)
summary(lmf)
plot_model(lmf, sort.est = TRUE)
lmlf = lm(Day_of_Year ~ Tmean_spring + Elevation + Latitude, data = df.NF.F)
summary(lmlf)
str(df.NF.F)

lmt = lmer(Day_of_Year ~ Tmean_spring + Latitude + Elevation + Tmean_winter + Prcp_spring + Swe_spring + Vp_spring +
             gdd_0_spring_accum + Longitude + (1|Scientific_Name), data = df.NF.T)
summary(lmt)
plot_model(lmt, sort.est = TRUE)
lmlt = lm(Day_of_Year ~ Tmean_spring + Elevation + Latitude, data = df.NF.T)
summary(lmlt)

lma = lmer(Day_of_Year ~ Tmean_spring + Latitude + Elevation + Tmean_winter + Prcp_spring + Swe_spring + Vp_spring +
             gdd_0_spring_accum + Longitude + (1|Scientific_Name), data = df.AL.F)
summary(lma)
plot_model(lma, sort.est = TRUE)
lmla = lm(Day_of_Year ~ Tmean_spring + Elevation + Latitude, data = df.AL.F)
summary(lmla)

# Read in geospatial datasets for inlabru models-work on this

NF.Forb = subset(df.NF.F, select = c("Day_of_Year", "Tmean_spring", "Latitude", "Longitude",
                                    "Scientific_Name", "Year", "Elevation", 
                                    "Tmean_winter", "Prcp_spring", "Swe_spring", "Vp_spring", "gdd_0_spring_accum",
                                    "Latx", "Logy", "Dataset"))
NF.Forb$Growth_Habit = "Forb"

#nftl2$Latitude = 43.5
#NF.Tree1 = rbind(nfth2, nftl2)
NF.Tree <- subset(df.NF.T, select = c("Day_of_Year", "Tmean_spring", "Latitude", "Longitude",
                                      "Scientific_Name", "Year", "Elevation", 
                                      "Tmean_winter", "Prcp_spring", "Swe_spring", "Vp_spring", "gdd_0_spring_accum",
                                      "Latx", "Logy", "Dataset"))
NF.Tree$Growth_Habit = "Tree"

AL.Forb <- subset(df.AL.F, select = c("Day_of_Year", "Tmean_spring", "Latitude", "Longitude",
                                     "Scientific_Name", "Year", "Elevation", 
                                     "Tmean_winter", "Prcp_spring", "Swe_spring", "Vp_spring", "gdd_0_spring_accum",
                                     "Latx", "Logy"))
AL.Forb$Growth_Habit = "Alpine"

##### JAGS models #####

# Model with common intercept, fixed effects for spring temperature and elevation, and species random 
# effects. This model is the one presented in the manuscript. Parameter estimates from other versions of
# this model are described in the manuscript and presented elsewhere, but are not included here for the
# sake of space.

#Broad models with just herbs, trees, and alpine spp.

# Model:
mod <- "model
{
  for(k in 1:Ntot) {
  FLD[k] ~ dnorm(mu[k],tau)
  mu[k] <- b0 + b*SpT[k] + b2*elev[k] + b3*Lat[k] + b4*Long[k] + a1[spnumber[k]] + a2[datan[k]]
  } #End k loop
  
  for(l in 1:N.spp){
    a1[l] ~ dnorm(0, 0.001)
  } #End l loop
  
  for(i in 1:dataset){
    a2[i] ~ dnorm(0, 0.001)
  } #End i loop
  
  b ~ dnorm(0, 0.001)
  b0 ~ dnorm(0, 0.001)
  b2 ~ dnorm(0, 0.001)
  b3 ~ dnorm(0, 0.001)
  b4 ~ dnorm(0, 0.001)
  tau <- sigma^-2
  sigma ~ dunif(0,100)
}"
  write(mod, "model.txt")
  
  params = c("a1", "a2", "b", "b0", "b2", "b3", "b4", "tau", "sigma", "mu") #parameters to monitor
  
  # List of observations by country and lifeform
  model.vect <- list(df.NF.F, df.NF.T)
  model.vect2 <- list("df.NF.F", "df.NF.T")
  
  # Model loop
  
  SPT.model.latents <- data.frame(Species_Type = rep(c('Northern Forest Flower'), each = 1),
                                  Growth_Habit = rep(c('forb', 'tree'), 1),
                                  n = NA, DIC = NA, r2 = NA, 
                                  a1.mean = NA, a1.sd = NA, a1.CI2.5 = NA, a1.CI97.5 = NA,
                                  a2.mean = NA, a2.sd = NA, a2.CI2.5 = NA, a2.CI97.5 = NA,
                                  b.mean = NA, b.sd = NA, b.CI2.5 = NA, b.CI97.5 = NA,
                                  b2.mean = NA, b2.sd = NA, b2.CI2.5 = NA, b2.CI97.5 = NA,
                                  b3.mean = NA, b3.sd = NA, b3.CI2.5 = NA, b3.CI97.5 = NA,
                                  b4.mean = NA, b4.sd = NA, b4.CI2.5 = NA, b4.CI97.5 = NA,
                                  b0.mean = NA, b0.sd = NA, b0.CI2.5 = NA, b0.CI97.5 = NA)
                                  
  
  for(i in 1:3){
    print(Sys.time()) # Track run time
    print(i) # Track iterations
    
    df <- model.vect[[i]]
    
    spnumber = as.numeric(factor(df$Scientific_Name))
    N.spp = max(spnumber)
    
    Ntot = dim(df)[1]
    
    SPT.model.latents$n[i] <- Ntot
    
    datan = as.numeric(factor(df$Dataset))
    dataset = max(datan)
    
    #input for JAGS
    input = list(Ntot = Ntot, N.spp = N.spp, spnumber = as.numeric(factor(df$Scientific_Name)), datan = as.numeric(factor(df$Dataset)),
                 dataset = max(datan), FLD = df$Day_of_Year, 
                 SpT = as.vector(df$Tmean_spring), elev = as.vector(df$Elevation), Lat = as.vector(df$Latitude), Long = as.vector(df$Longitude)) #input data
    
    mod.FLD.Spring <- jags(model = "model.txt", data = input, param = params,
                           n.chains = 3, #number of separate MCMC chains
                           n.iter = 5000, #number of iterations per chain
                           n.burnin = 1000) #number of initial iterations to discard
    SPT.model.latents$DIC[i] <- DIC(mod.FLD.Spring)
    
    attach.jags(mod.FLD.Spring)
    
    muFLD = colMeans(mu)
    SPT.model.latents$r2[i] <- summary(lm(muFLD ~ df$Day_of_Year)$r.squared) 
    
    # Latent posteriors
    SPT.model.latents$b.mean[i] = mean(b)
    SPT.model.latents$b.sd[i] = sd(b)
    SPT.model.latents$b.CI2.5[i] = quantile(b, probs = 0.025, names = F)
    SPT.model.latents$b.CI97.5[i] = quantile(b, probs = 0.975, names = F)
    
    SPT.model.latents$b2.mean[i] = mean(b2)
    SPT.model.latents$b2.sd[i] = sd(b2)
    SPT.model.latents$b2.CI2.5[i] = quantile(b2, probs = 0.025, names = F)
    SPT.model.latents$b2.CI97.5[i] = quantile(b2, probs = 0.975, names = F)
    
    SPT.model.latents$b3.mean[i] = mean(b3)
    SPT.model.latents$b3.sd[i] = sd(b3)
    SPT.model.latents$b3.CI2.5[i] = quantile(b3, probs = 0.025, names = F)
    SPT.model.latents$b3.CI97.5[i] = quantile(b3, probs = 0.975, names = F)
    
    SPT.model.latents$b4.mean[i] = mean(b4)
    SPT.model.latents$b4.sd[i] = sd(b4)
    SPT.model.latents$b4.CI2.5[i] = quantile(b4, probs = 0.025, names = F)
    SPT.model.latents$b4.CI97.5[i] = quantile(b4, probs = 0.975, names = F)
    
    SPT.model.latents$b0.mean[i] = mean(b0)
    SPT.model.latents$b0.sd[i] = sd(b0)
    SPT.model.latents$b0.CI2.5[i] = quantile(b0, probs = 0.025, names = F)
    SPT.model.latents$b0.CI97.5[i] = quantile(b0, probs = 0.975, names = F)
    
    SPT.model.latents$a1.mean[i] = mean(a1)
    SPT.model.latents$a1.sd[i] = sd(a1)
    SPT.model.latents$a1.CI2.5[i] = quantile(a1, probs = 0.025, names = F)
    SPT.model.latents$a1.CI97.5[i] = quantile(a1, probs = 0.975, names = F)
 
    SPT.model.latents$a2.mean[i] = mean(a2)
    SPT.model.latents$a2.sd[i] = sd(a2)
    SPT.model.latents$a2.CI2.5[i] = quantile(a2, probs = 0.025, names = F)
    SPT.model.latents$a2.CI97.5[i] = quantile(a2, probs = 0.975, names = F)
  
    # Species-level hierarchical posteriors
    df.sp <- data.frame(Species_Type = SPT.model.latents$Species_Type[i], Growth_Habit = SPT.model.latents$Growth_Habit[i], 
                        Sp = 1:N.spp,
                        a1.mean = NA, a1.sd = NA, a1.CI2.5 = NA, a1.CI97.5 = NA) 
    
    for(j in 1:N.spp){
      
      df.sp$a1.mean[j] <- mean(a1[,j])
      df.sp$a1.sd[j] = sd(a1[,j])
      df.sp$a1.CI2.5[j] = quantile(a1[,j], probs = 0.025, names = F)
      df.sp$a1.CI97.5[j] = quantile(a1[,j], probs = 0.975, names = F)
    }
    
    # Dataset-level hierarchical posteriors
    df.set <- data.frame(Species_Type = SPT.model.latents$Species_Type[i], Growth_Habit = SPT.model.latents$Growth_Habit[i], 
                        Set = 1:dataset,
                        a2.mean = NA, a2.sd = NA, a2.CI2.5 = NA, a2.CI97.5 = NA) 
    
    for(j in 1:dataset){
      
      df.set$a2.mean[j] <- mean(a2[,j])
      df.set$a2.sd[j] = sd(a2[,j])
      df.set$a2.CI2.5[j] = quantile(a2[,j], probs = 0.025, names = F)
      df.set$a2.CI97.5[j] = quantile(a2[,j], probs = 0.975, names = F)
    }
    
    write.csv(df.sp, paste0(model.vect2[i],".Sp.Post.csv"))
    
    write.csv(df.set, paste0(model.vect2[i],".Sp.Post.csv"))
    detach.jags()
  }
  
  write.csv(SPT.model.latents, paste0("SpT.model.Latent.Post.csv"))
  
###end run here
   
  ##### Preparing meshes for inlabru models ##########
  
  max.edge = 0.5 # Sets edge length for spatial autocorrelation analysis
  
  # EU Mesh
  
  head(NF.Forb)
  head(NF.Tree)
  
  
  df.NF <- rbind(NF.Forb, NF.Tree, AL.Forb)
  df.NF <- subset(df.NF, Logy < 0)
  df.NF$order <- factor(df.NF$Growth_Habit, levels = c("Tree", "Alpine", "Forb"))
  
  mesh.NF <- inla.mesh.2d(
    loc = df.NF[, c('Logy', 'Latx')],
    #offset = c(0.5, 1.5),
    max.edge = c(max.edge, max.edge*3),
    # discretization accuracy
    cutoff = max.edge/5) # cutoff removes locations that are too close, good to have >0
  
  mesh.NF.gg <- ggplot() + theme_bw() +
    gg(mesh.NF,
       edge.color = "grey50",
       int.color = "black",
       ext.color = "black"
    ) +
    geom_point(data = df.NF, aes(x = Logy, y = Latx, color = order)) +
    scale_color_manual(name = "Group", labels = c('Trees and Wildflowers', 'Alpine', ''), values = c('red', 'green', 'white')) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme(axis.text = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          axis.title = element_text(size = 16))
  #tiff("map_group.tiff", units="in", width=8, height=6, res=600)  
  mesh.NF.gg
  dev.off()
  
  all_plot = ggplot(df.NF, aes(x = Tmean_spring, y = Day_of_Year, color = order)) +
    geom_point(size = 2.5, alpha = 0.1) +
    scale_color_brewer(name = "Group", palette = "Set1", label = c("Tree", "Alpine", "Woodland Herb")) +
    geom_smooth(method = "lm", se = FALSE, lwd = 1.5) +
    ylab("Day of Year") +
    xlab("Mean Spring Temperature (\u00B0C)") +
    xlim(-1,15) +
    theme_bw() +
    theme(axis.text = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          axis.title = element_text(size = 16))

  tiff("group.tiff", units="in", width=8, height=5, res=600)  
  all_plot
  dev.off()
  ##### inlabru models #####
  
  # As above, we are not including all model iterations in this documentation -- we have only included
  # examples of two model structures: one that is exactly the same as the JAGS model above but using
  # INLA/inlabru for parameter estimates and one that is the same as above, but with the addition of a 
  # spatial autocorrelation term in the model structure. Furthermore, we have only included the model for
  # European trees as an example, but the code can be easily modified for the other continent x lifeform
  # combinations that are included in the dataset.
  
  # inlabru model without spatial autocorrelation (same as JAGS model above)
  
  comp.SpT.elev <- Day_of_Year ~ 1 + springT(Tmean_spring, model="linear") +
    species(Scientific_Name, model="factor_full") +
    elev(Elevation, model = "linear")
  
  # inlabru model with spatial autocorrelation
  
  comp.SpT.elev.U <- Day_of_Year ~ 1 + springT(Tmean_spring, model="linear") +
    U(cbind(Longitude, Latitude), model=matern) +
    species(Scientific_Name, model="factor_full") +
    elev(Elevation, model = "linear")
  
  # NF tree examples:
  
  # Define matern
  matern <-
    inla.spde2.pcmatern(mesh.NF,
                        prior.sigma = c(10, 0.01),
                        prior.range = c(1, 0.01)
    )
  
  
  # Fitting the models with inlabru:
  
  fit.EU.T.SpT.elev <- bru(comp.SpT.elev, NF.Tree, family = "gaussian", 
                           options=list(inla.mode = "experimental"))
  fit.EU.T.SpT.elev.U <- bru(comp.SpT.elev.U, NF.Tree, family = "gaussian", 
                             options=list(inla.mode = "experimental"))
  
  # Checking DIC:
  
  summary(fit.EU.T.SpT.elev)
  summary(fit.EU.T.SpT.elev.U)
  
  # Extracting posterior estimates for intercept and slopes
  
  EU.T.SpT.elev.beta <- fit.EU.T.SpT.elev$summary.fixed[1,]
  EU.T.SpT.elev.int <- fit.EU.T.SpT.elev$summary.fixed[3,]
  EU.T.SpT.elev.elev <- fit.EU.T.SpT.elev$summary.fixed[2,]
  
  EU.T.SpT.elev.U.beta <- fit.EU.T.SpT.elev.U$summary.fixed[1,]
  EU.T.SpT.elev.U.int <- fit.EU.T.SpT.elev.U$summary.fixed[3,]
  EU.T.SpT.elev.U.elev <- fit.EU.T.SpT.elev.U$summary.fixed[2,]
  
  # Extracting spatial autocorrelation information for spatial autocorrelation model:
  
  EU.T.SpT.elev.U.SA <- fit.EU.T.SpT.elev.U$summary.random$U
  
  #write.csv(EU.T.SpT.elev.U.SA, 'EU.T.SpT.elev.U SA posteriors.csv')
  
  
  # Extracting species-level random effects
  
  EU.T.SpT.elev.species <- fit.EU.T.SpT.elev$summary.random$species
  EU.T.SpT.elev.U.species <- fit.EU.T.SpT.elev.U$summary.random$species
  
  EU.T.species.REs <- rbind(EU.T.SpT.elev.species, EU.T.SpT.elev.U.species)
  
  EU.T.species.REs$model <- rep(c('SpT.elev', 'SpT.elev.U'), each = 6)
  #write.csv(EU.T.species.REs, 'EU.T species random effects.csv')
  
  # Evaluating pred vs obs r2:
  
  fit.EU.T.SpT.elev.r2.df <- data.frame(DOY.obs = NF.Tree$Day_of_Year, 
                                        DOY.proj = fit.EU.T.SpT.elev$summary.fitted.values$mean[1:nrow(NF.Tree)])
  summary(lm(DOY.obs ~ DOY.proj, data = fit.EU.T.SpT.elev.r2.df))
  
  fit.EU.T.SpT.elev.U.r2.df <- data.frame(DOY.obs = NF.Tree$Day_of_Year, 
                                          DOY.proj = fit.EU.T.SpT.elev.U$summary.fitted.values$mean[1:nrow(NF.Tree)])
  summary(lm(DOY.obs ~ DOY.proj, data = fit.EU.T.SpT.elev.U.r2.df))
  
  # Exporting posteriors as csvs:
  
  EU.T.post.beta <- rbind(EU.T.SpT.elev.beta, EU.T.SpT.elev.U.beta)
  EU.T.post.beta$model <- c('SpT.elev', 'SpT.elev.U')
  EU.T.post.beta$lifeform <- 'nativetree'
  EU.T.post.beta$Region <- 'Europe'
  EU.T.post.beta$Param <- 'B.SpT'
  
  
  EU.T.post.int <- rbind(EU.T.SpT.elev.int, EU.T.SpT.elev.U.int)
  EU.T.post.int$model <- c('SpT.elev', 'SpT.elev.U')
  EU.T.post.int$lifeform <- 'nativetree'
  EU.T.post.int$Region <- 'Europe'
  EU.T.post.int$Param <- 'int'
  
  
  EU.T.post.B2 <- rbind(EU.T.SpT.elev.elev, EU.T.SpT.elev.U.elev)
  EU.T.post.B2$model <- c('SpT.elev', 'SpT.elev.U')
  EU.T.post.B2$lifeform <- 'nativetree'
  EU.T.post.B2$Region <- 'Europe'
  EU.T.post.B2$Param <- 'B.2'
  
  EU.T.post <- rbind(EU.T.post.beta, EU.T.post.int, EU.T.post.B2)
  #write.csv(EU.T.post, 'EU.T param posteriors.csv')
  
  ##### JAGS models ##### per species
  
  # Model with common intercept, fixed effects for spring temperature and elevation, and species random 
  # effects. This model is the one presented in the manuscript. Parameter estimates from other versions of
  # this model are described in the manuscript and presented elsewhere, but are not included here for the
  # sake of space.
  
#Models seperated by Latitude 

  df.NF.Fl <- subset(df.NF.F, Latx < 38)
  sd(df.NF.Fl$Tmean_spring)
  sd(df.NF.Fl$Elevation)
  sd(df.NF.Fl$Latitude)
  df.NF.Tl <- subset(df.NF.T, Latx < 38)
  sd(df.NF.Tl$Tmean_spring)
  sd(df.NF.Tl$Elevation)
  sd(df.NF.Tl$Latitude)
  df.AL.Fl <- subset(df.AL.F, Latx < 38)
  
  df.NF.Fm <- subset(df.NF.F, Latx > 38 & Latx < 42)
  sd(df.NF.Fm$Tmean_spring)
  
  df.NF.Tm <- subset(df.NF.T, Latx > 38 & Latx < 42)
  sd(df.NF.Tm$Tmean_spring)
  sd(df.NF.Fm$Elevation)
  df.AL.Fm <- subset(df.AL.F, Latx > 38 & Latx < 42)
  sd(df.NF.Tm$Tmean_spring)
  sd(df.NF.Tm$Elevation)
  
  df.NF.Fh <- subset(df.NF.F, Latx > 42)
  sd(df.NF.Fh$Tmean_spring)
  sd(df.NF.Fh$Elevation)
  df.NF.Th <- subset(df.NF.T, Latx > 42)
  sd(df.NF.Th$Tmean_spring)
  sd(df.NF.Th$Elevation)
  df.AL.Fh <- subset(df.AL.F, Latx > 42)
  sd(df.AL.Fh$Tmean_spring)
  sd(df.AL.Fh$Elevation)
  
  # Model:
  mod <- "model
{
  for(k in 1:Ntot) {
  FLD[k] ~ dnorm(mu[k],tau)
  mu[k] <- b0 + b*SpT[k] + b2*elev[k] + b3*Lat[k] + a[spnumber[k]] 
  } #End k loop
  
  for(l in 1:N.spp){
    a[l] ~ dnorm(0, 0.001)
  } #End l loop
  
  b ~ dnorm(0, 0.001)
  b0 ~ dnorm(0, 0.001)
  b2 ~ dnorm(0, 0.001)
  b3 ~ dnorm(0, 0.001)
  tau <- sigma^-2
  sigma ~ dunif(0,100)
}"
  write(mod, "model.txt")
  
  params = c("a", "b", "b0", "b2", "b3", "tau", "sigma", "mu") #parameters to monitor
  
  # List of observations by country and lifeform
  model.vect <- list(df.NF.Fl, df.NF.Tl, df.AL.Fl, df.NF.Fm, df.NF.Tm, df.AL.Fm, df.NF.Fh, df.NF.Th, df.AL.Fh)
  model.vect2 <- list("df.NF.Fl", "df.NF.Tl", "df.AL.Fl", "df.NF.Fm", "df.NF.Tm", "df.AL.Fm", "df.NF.Fh", "df.NF.Th", "df.AL.Fh")
  
  # Model loop
  
  SPT.model.latents <- data.frame(Species_Type = c('low', 'low', 'low', 'mid', 'mid', 'mid', 'high', 'high', 'high'),
                                  Growth_Habit = rep(c('forb', 'tree', 'alpine'), 3),
                                  n = NA, DIC = NA, r2 = NA, 
                                  a.mean = NA, a.sd = NA, a.CI2.5 = NA, a.CI97.5 = NA,
                                  b.mean = NA, b.sd = NA, b.CI2.5 = NA, b.CI97.5 = NA,
                                  b2.mean = NA, b2.sd = NA, b2.CI2.5 = NA, b2.CI97.5 = NA,
                                  b3.mean = NA, b3.sd = NA, b3.CI2.5 = NA, b3.CI97.5 = NA,
                                  b0.mean = NA, b0.sd = NA, b0.CI2.5 = NA, b0.CI97.5 = NA)
  
  
  for(i in 1:9){
    print(Sys.time()) # Track run time
    print(i) # Track iterations
    
    df <- model.vect[[i]]
    
    spnumber = as.numeric(factor(df$Scientific_Name))
    N.spp = max(spnumber)
    
    Ntot = dim(df)[1]
    
    SPT.model.latents$n[i] <- Ntot
    
    #input for JAGS
    input = list(Ntot = Ntot, N.spp = N.spp, spnumber = as.numeric(factor(df$Scientific_Name)), FLD = df$Day_of_Year, 
                 SpT = as.vector(df$Tmean_spring), elev = as.vector(df$Elevation), Lat = as.vector(df$Latitude)) #input data
    
    mod.FLD.Spring <- jags(model = "model.txt", data = input, param = params,
                           n.chains = 3, #number of separate MCMC chains
                           n.iter = 100000, #number of iterations per chain
                           n.burnin = 30000) #number of initial iterations to discard
    SPT.model.latents$DIC[i] <- DIC(mod.FLD.Spring)
    
    attach.jags(mod.FLD.Spring)
    
    muFLD = colMeans(mu)
    SPT.model.latents$r2[i] <- summary(lm(muFLD ~ df$Day_of_Year)$r.squared) 
    
    # Latent posteriors
    SPT.model.latents$b.mean[i] = mean(b)
    SPT.model.latents$b.sd[i] = sd(b)
    SPT.model.latents$b.CI2.5[i] = quantile(b, probs = 0.025, names = F)
    SPT.model.latents$b.CI97.5[i] = quantile(b, probs = 0.975, names = F)
    
    SPT.model.latents$b2.mean[i] = mean(b2)
    SPT.model.latents$b2.sd[i] = sd(b2)
    SPT.model.latents$b2.CI2.5[i] = quantile(b2, probs = 0.025, names = F)
    SPT.model.latents$b2.CI97.5[i] = quantile(b2, probs = 0.975, names = F)
    
    SPT.model.latents$b3.mean[i] = mean(b3)
    SPT.model.latents$b3.sd[i] = sd(b3)
    SPT.model.latents$b3.CI2.5[i] = quantile(b3, probs = 0.025, names = F)
    SPT.model.latents$b3.CI97.5[i] = quantile(b3, probs = 0.975, names = F)
    
    SPT.model.latents$b0.mean[i] = mean(b0)
    SPT.model.latents$b0.sd[i] = sd(b0)
    SPT.model.latents$b0.CI2.5[i] = quantile(b0, probs = 0.025, names = F)
    SPT.model.latents$b0.CI97.5[i] = quantile(b0, probs = 0.975, names = F)
    
    SPT.model.latents$a.mean[i] = mean(a)
    SPT.model.latents$a.sd[i] = sd(a)
    SPT.model.latents$a.CI2.5[i] = quantile(a, probs = 0.025, names = F)
    SPT.model.latents$a.CI97.5[i] = quantile(a, probs = 0.975, names = F)
    
    # Species-level hierarchical posteriors
    df.sp <- data.frame(Species_Type = SPT.model.latents$Species_Type[i], Growth_Habit = SPT.model.latents$Growth_Habit[i], 
                        Sp = 1:N.spp,
                        a.mean = NA, a.sd = NA, a.CI2.5 = NA, a.CI97.5 = NA) 
    
    for(j in 1:N.spp){
      
      df.sp$a.mean[j] <- mean(a[,j])
      df.sp$a.sd[j] = sd(a[,j])
      df.sp$a.CI2.5[j] = quantile(a[,j], probs = 0.025, names = F)
      df.sp$a.CI97.5[j] = quantile(a[,j], probs = 0.975, names = F)
    }
    
    write.csv(df.sp, paste0(model.vect2[i],".Sp.Post19.csv"))
    detach.jags()
  }
  
  write.csv(SPT.model.latents, paste0("SpT.model.Latent.Post_9class.csv"))
  
#Models per species
  
  # Model:
  mod <- "model
{
  for(k in 1:Ntot) {
  FLD[k] ~ dnorm(mu[k],tau)
  mu[k] <- b0 + b*SpT[k] + b2*elev[k] + b3*Lat[k] + a[spnumber[k]] 
  } #End k loop
  
  for(l in 1:N.spp){
    a[l] ~ dnorm(0, 0.001)
  } #End l loop
  
  b ~ dnorm(0, 0.001)
  b0 ~ dnorm(0, 0.001)
  b2 ~ dnorm(0, 0.001)
  b3 ~ dnorm(0, 0.001)
  tau <- sigma^-2
  sigma ~ dunif(0,100)
}"
  write(mod, "model.txt")
  
  params = c("a", "b", "b0", "b2", "b3", "tau", "sigma", "mu") #parameters to monitor
  
  # List of observations by country and lifeform
  model.vect <- list(df.AL.F1, df.AL.F2, df.AL.F3, df.AL.F4, df.AL.F5, df.AL.F6, 
                     df.NF.F1, df.NF.F2, df.NF.F3, df.NF.F4, df.NF.F5, df.NF.F6, df.NF.F7, df.NF.F8, df.NF.F9, df.NF.F10,
                     df.NF.T1, df.NF.T2, df.NF.T3, df.NF.T4, df.NF.T5, df.NF.T6, df.NF.T7, df.NF.T8, df.NF.T9, df.NF.T10, df.NF.T11, df.NF.T12, df.NF.T13, df.NF.T14, df.NF.T15)
  model.vect2 <- list("df.AL.F1", "df.AL.F2", "df.AL.F3", "df.AL.F4", "df.AL.F5", "df.AL.F6", 
                      "df.NF.F1", "df.NF.F2", "df.NF.F3", "df.NF.F4", "df.NF.F5", "df.NF.F6", "df.NF.F7", "df.NF.F8", "df.NF.F9", "df.NF.F10",
                      "df.NF.T1", "df.NF.T2", "df.NF.T3", "df.NF.T4", "df.NF.T5", "df.NF.T6", "df.NF.T7", "df.NF.T8", "df.NF.T9", "df.NF.T10", "df.NF.T11", "df.NF.T12", "df.NF.T13", "df.NF.T14", "df.NF.T15")
  
  # Model loop
  
  SPT.model.latents <- data.frame(Species_Type = c('CABI', 'DILA', 'GEPE', 'RHGR', 'VAUL', 'VAVI',
                                                   'CLBO', 'COCA', 'ERAM', 'MACA', 'OXMO', 'SACA', 'TRBO', 'TRER', 'TRUN', 'VILA',
                                                   'ACPE', 'ACRU', 'ACSA', 'ACSP', 'BEAL', 'BELE', 'BEPA', 'FAGR', 'FRAM', 'OSVI', 'PRPE', 'PRSE', 'QURU', 'SOAM', 'TIAM'),
                                  Growth_Habit = c('alpine', 'alpine', 'alpine', 'alpine', 'alpine', 'alpine',
                                                   'forb', 'forb', 'forb', 'forb', 'forb', 'forb', 'forb', 'forb', 'forb', 'forb',
                                                   'tree', 'tree', 'tree', 'tree', 'tree', 'tree', 'tree', 'tree', 'tree', 'tree', 'tree', 'tree', 'tree', 'tree', 'tree'),
                                  n = NA, DIC = NA, r2 = NA, 
                                  a.mean = NA, a.sd = NA, a.CI2.5 = NA, a.CI97.5 = NA,
                                  b.mean = NA, b.sd = NA, b.CI2.5 = NA, b.CI97.5 = NA,
                                  b2.mean = NA, b2.sd = NA, b2.CI2.5 = NA, b2.CI97.5 = NA,
                                  b3.mean = NA, b3.sd = NA, b3.CI2.5 = NA, b3.CI97.5 = NA,
                                  b0.mean = NA, b0.sd = NA, b0.CI2.5 = NA, b0.CI97.5 = NA)
  
  
  for(i in 1:31){
    print(Sys.time()) # Track run time
    print(i) # Track iterations
    
    df <- model.vect[[i]]
    
    spnumber = as.numeric(factor(df$Scientific_Name))
    N.spp = max(spnumber)
    
    Ntot = dim(df)[1]
    
    SPT.model.latents$n[i] <- Ntot
    
    #input for JAGS
    input = list(Ntot = Ntot, N.spp = N.spp, spnumber = as.numeric(factor(df$Scientific_Name)), FLD = df$Day_of_Year, 
                 SpT = as.vector(df$Tmean_spring), elev = as.vector(df$Elevation), Lat = as.vector(df$Latitude)) #input data
    
    mod.FLD.Spring <- jags(model = "model.txt", data = input, param = params,
                           n.chains = 3, #number of separate MCMC chains
                           n.iter = 100000, #number of iterations per chain
                           n.burnin = 30000) #number of initial iterations to discard
    SPT.model.latents$DIC[i] <- DIC(mod.FLD.Spring)
    
    attach.jags(mod.FLD.Spring)
    
    muFLD = colMeans(mu)
    SPT.model.latents$r2[i] <- summary(lm(muFLD ~ df$Day_of_Year)$r.squared) 
    
    # Latent posteriors
    SPT.model.latents$b.mean[i] = mean(b)
    SPT.model.latents$b.sd[i] = sd(b)
    SPT.model.latents$b.CI2.5[i] = quantile(b, probs = 0.025, names = F)
    SPT.model.latents$b.CI97.5[i] = quantile(b, probs = 0.975, names = F)
    
    SPT.model.latents$b2.mean[i] = mean(b2)
    SPT.model.latents$b2.sd[i] = sd(b2)
    SPT.model.latents$b2.CI2.5[i] = quantile(b2, probs = 0.025, names = F)
    SPT.model.latents$b2.CI97.5[i] = quantile(b2, probs = 0.975, names = F)
    
    SPT.model.latents$b3.mean[i] = mean(b3)
    SPT.model.latents$b3.sd[i] = sd(b3)
    SPT.model.latents$b3.CI2.5[i] = quantile(b3, probs = 0.025, names = F)
    SPT.model.latents$b3.CI97.5[i] = quantile(b3, probs = 0.975, names = F)
    
    SPT.model.latents$b0.mean[i] = mean(b0)
    SPT.model.latents$b0.sd[i] = sd(b0)
    SPT.model.latents$b0.CI2.5[i] = quantile(b0, probs = 0.025, names = F)
    SPT.model.latents$b0.CI97.5[i] = quantile(b0, probs = 0.975, names = F)
    
    SPT.model.latents$a.mean[i] = mean(a)
    SPT.model.latents$a.sd[i] = sd(a)
    SPT.model.latents$a.CI2.5[i] = quantile(a, probs = 0.025, names = F)
    SPT.model.latents$a.CI97.5[i] = quantile(a, probs = 0.975, names = F)
    
    # Species-level hierarchical posteriors
    df.sp <- data.frame(Species_Type = SPT.model.latents$Species_Type[i], Growth_Habit = SPT.model.latents$Growth_Habit[i], 
                        Sp = 1:N.spp,
                        a.mean = NA, a.sd = NA, a.CI2.5 = NA, a.CI97.5 = NA) 
    
    for(j in 1:N.spp){
      
      df.sp$a.mean[j] <- mean(a[,j])
      df.sp$a.sd[j] = sd(a[,j])
      df.sp$a.CI2.5[j] = quantile(a[,j], probs = 0.025, names = F)
      df.sp$a.CI97.5[j] = quantile(a[,j], probs = 0.975, names = F)
    }
    
    write.csv(df.sp, paste0(model.vect2[i],".Sp.Post1_sp.csv"))
    detach.jags()
  }
  
  write.csv(SPT.model.latents, paste0("SpT.model.Latent.Post_spclass.csv"))
  
#Need to run models by common focal species
#Need to run models by year
#look at possibly important variables - 
    #looked at prcp, vp, winter temp, gdd - need to look at solar radiation, daylength, chilling degrees, etc. 
   
  ####Figures - look at spring temp trends and plot group differences
  spt = read.csv("ts_sense.csv", header = TRUE)
  spt$order <- factor(spt$Species_Type, levels = c("North",
                                           "Mid-Atlantic",
                                           "South", "Full"))
  spt$order1 <- factor(spt$Growth_Habit, levels = c("Understory",
                                                   "Canopy"))
  spt$order2 <- factor(spt$parameter, levels = c("Temperature",
                                                    "Elevation", "Latitude"))
  
#tiff(file = "mismatch.tiff", width = 7, height = 6.5, units = 'in', res = 600, pointsize = 11)
 #install.packages("ggh4x") 
 #install.packages("rlang")
   
spt_plot = ggplot(data = spt, aes(x = order, y = y, colour = order1)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.3, position = "dodge") +
  #geom_line(aes(group = order1), position = position_dodge(width = 0.2), lwd = 1) +
  geom_point(aes(colour = order1), size = 3, position = position_dodge(width = 0.3)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    scale_colour_viridis_d(name = "Functional Group") +
    ylab("Sensitivity (days/\u00b0C)") +
    ylim(-13,0) +
    xlab("") +
  ggtitle("Spring Temperature") +
  theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 16),
          plot.title = element_text(size = 16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.position="none",
          strip.placement = "outside")
          #strip.background = element_blank(),
          #strip.text.x = element_blank())
spt_plot 

spt1 = read.csv("ts_sense1.csv", header = TRUE)
spt1$order <- factor(spt1$Species_Type, levels = c("North",
                                                 "Mid-Atlantic",
                                                 "South", "Full"))
spt1$order1 <- factor(spt1$Growth_Habit, levels = c("Understory",
                                                  "Canopy"))

spt_plot1 = ggplot(data = spt1, aes(x = order, y = y, colour = order1)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.3, position = "dodge") +
  #geom_line(aes(group = order1), position = position_dodge(width = 0.2), lwd = 1) +
  geom_point(aes(colour = order1), size = 3, position = position_dodge(width = 0.3)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_colour_viridis_d(name = "Functional Group") +
  ylab("Sensitivity (days/100 m)") +
  ggtitle("Elevation") +
  ylim(-2,12) +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position="none",
        strip.placement = "outside")
#strip.background = element_blank(),
#strip.text.x = element_blank())
spt_plot1  

spt2 = read.csv("ts_sense2.csv", header = TRUE)
spt2$order <- factor(spt2$Species_Type, levels = c("North",
                                                   "Mid-Atlantic",
                                                   "South", "Full"))
spt2$order1 <- factor(spt2$Growth_Habit, levels = c("Understory",
                                                    "Canopy"))

spt_plot2 = ggplot(data = spt2, aes(x = order, y = y, colour = order1)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.3, position = "dodge") +
  #geom_line(aes(group = order1), position = position_dodge(width = 0.2), lwd = 1) +
  geom_point(aes(colour = order1), size = 3, position = position_dodge(width = 0.3)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_colour_viridis_d(name = "Functional Group") +
  ylab("Sensitivity (days/\u00b0N)") +
  ggtitle("Latitiude") +
  #ylim(-13,0) +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position="none",
        strip.placement = "outside")
#strip.background = element_blank(),
#strip.text.x = element_blank())
spt_plot2

spt_p = plot_grid(spt_plot, spt_plot1, spt_plot2, ncol = 3, 
                  labels = c("A", "B", "C"))
spt_p

legend <- get_legend(spt_plot + guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom"))

spt_p1 = plot_grid(spt_p, legend, ncol = 1, rel_heights = c(1, .1))

tiff(file = "mismatch.tiff", width = 10, height = 5, units = 'in', res = 600, pointsize = 11)
spt_p1
dev.off()

  