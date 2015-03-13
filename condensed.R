library(ggplot2)
library(plyr)

# Helper functions
source("~/Code/R-me-hearties/R/multiplot.R")
source("~/Code/R-me-hearties/R/summarySE.R")

# Factors
regionfactors = c("Light Scene\nLight Avatar", "Light Scene\nHeavy Avatar", "Heavy Scene\nLight Avatar", "Heavy Scene\nHeavy Avatar")
breakthing = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150)

# Data - Avatars
avatar_regionfactors = c("Light Inventory\nLight Scene", "Light Inventory\nHeavy Scene", "Heavy Inventory\nLight Scene", "Heavy Inventory\nHeavy Scene")
light_av_region_serv = c(18.75, 26.30, 134.39, 77.85)
heavy_av_region_serv = c(24.58, 32.01, 102.75, 103.59)
light_av_region_serv_std = c(0.29, 0.32, 2.83, 12.94) 
heavy_av_region_serv_std = c(0.71, 0.60, 42.55, 12.64)

# Data - Scenes
scene_regionfactors = c("Light Inventory\nLight Avatar", "Light Inventory\nHeavy Avatar", "Heavy Inventory\nLight Avatar", "Heavy Inventory\nHeavy Avatar")
light_sce_region_serv = c(18.75, 24.58, 134.39, 102.75)
heavy_sce_region_serv = c(26.30, 32.01, 77.85, 103.59)
light_sce_region_serv_std = c(0.29, 0.71, 2.83, 42.55) 
heavy_sce_region_serv_std = c(0.32, 0.60, 12.94, 12.64)

# Data - Inventories
light_inv_region_serv = c(18.75, 24.58, 26.30, 32.01)
light_inv_robust_serv = c(16.10, 20.55, 26.18, 30.65)
heavy_inv_region_serv = c(134.39, 102.75, 77.86, 103.59)
heavy_inv_robust_serv = c(14.90, 20.52, 28.10, 32.49)

# Standard Deviations - Inventories
light_inv_region_serv_std = c(0.29, 0.71, 0.32, 0.60)
light_inv_robust_serv_std = c(0.39, 0.25, 0.19, 0.47)
heavy_inv_region_serv_std = c(2.83, 42.55, 12.95, 12.65)
heavy_inv_robust_serv_std = c(0.85, 0.81, 0.24, 0.72)

basic_plot_by_factor <- function(dframe, plot_title) {
  p <- ggplot(data=dframe, aes(x=Configs,y=Y)) + 
    geom_bar(width=.5, stat="identity",position="dodge", colour="#5D92B1", fill="#5D92B1") +
    xlab("") + ylab("Total Processor Time (s)") + ggtitle(plot_title) + 
    geom_errorbar(aes(ymin=Y-Error, ymax=Y+Error), width=.2, position=position_dodge(.5)) +
    theme(axis.text.x = element_text(colour = 'black'), axis.text.y = element_text(colour='black'), text = element_text(size=18)) + 
    scale_y_continuous(limits=c(0,150),breaks=breakthing)
  return(p)
}

basic_compare_dataframe <- function(configuration_factors, y, std, plot_title) {
  df <- data.frame(Configs=factor(configuration_factors, levels=configuration_factors),
                   Y = y,
                   Error = std)
  return(basic_plot_by_factor(df, plot_title))
}

lightInvDF = data.frame(Configs=factor(regionfactors, levels=regionfactors),
                        Servers=factor(c(rep("Simulation Server",4),rep("Dedicated Server",4)), levels=c("Simulation Server", "Dedicated Server")),
                        Y=c(light_inv_region_serv,light_inv_robust_serv),
                        Error=c(light_inv_region_serv_std,light_inv_robust_serv_std))

heavyInvDF = data.frame(Configs=factor(regionfactors, levels=regionfactors),
                        Servers=factor(c(rep("Simulation Server",4),rep("Dedicated Server",4)), levels=c("Simulation Server", "Dedicated Server")),
                        Y=c(heavy_inv_region_serv,heavy_inv_robust_serv),
                        Error=c(heavy_inv_region_serv_std,heavy_inv_robust_serv_std))

lipb = basic_compare_dataframe(regionfactors, light_inv_region_serv, light_inv_region_serv_std, "Light Inventory\nPrivileged Processor Time")
hipb = basic_compare_dataframe(regionfactors, heavy_inv_region_serv, heavy_inv_region_serv_std, "Heavy Inventory\nPrivileged Processor Time")
lsp = basic_compare_dataframe(scene_regionfactors, light_sce_region_serv, light_sce_region_serv_std, "Light Scene\nPrivileged Processor Time")
hsp = basic_compare_dataframe(scene_regionfactors, heavy_sce_region_serv, heavy_sce_region_serv_std, "Heavy Scene\nPrivileged Processor Time")
lap = basic_compare_dataframe(avatar_regionfactors, light_av_region_serv, light_av_region_serv_std, "Light Avatar\nPrivileged Processor Time")
hap = basic_compare_dataframe(avatar_regionfactors, heavy_av_region_serv, heavy_av_region_serv_std, "Heavy Avatar\nPrivileged Processor Time")

lip <- ggplot(data=lightInvDF, aes(x=Configs,y=Y,fill=Servers)) + 
  geom_bar(width=.5, stat="identity",position="dodge") + 
  scale_fill_manual(name="Item retrieval from", values=c("#5D92B1", "#9BCD9B")) +
  xlab("") + ylab("Total Processor Time (s)") + ggtitle("Light User Inventory\nPrivileged Processor Time") +
  geom_errorbar(aes(ymin=Y-Error, ymax=Y+Error), width=.2, position=position_dodge(.5)) +
  theme(axis.text.x = element_text(colour = 'black'), axis.text.y = element_text(colour='black'), text = element_text(size=16)) + 
  scale_y_continuous(limits=c(0,150),breaks=breakthing) + theme(legend.position="top")

hip = ggplot(data=heavyInvDF, aes(x=Configs,y=Y,fill=Servers)) + 
  geom_bar(width=.5, stat="identity",position="dodge") + 
  scale_fill_manual(name="Item retrieval from", values=c("#5D92B1", "#9BCD9B")) +
  xlab("") + ylab("Total Processor Time (s)") + ggtitle("Heavy User Inventory\nPrivileged Processor Time") +
  geom_errorbar(aes(ymin=Y-Error, ymax=Y+Error), width=.2, position=position_dodge(.5)) +
  theme(axis.text.x = element_text(colour = 'black'), axis.text.y = element_text(colour='black'), text = element_text(size=16)) + 
  scale_y_continuous(limits=c(0,150),breaks=breakthing) + theme(legend.position="top")

multiplot(lsp, hsp, cols=2)
#multiplot(lip,hip,cols=2)