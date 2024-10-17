#Code to analyze snowpit data from Denali National Park and create Fig.3b&c
#Accompanies Prugh et al. Landscape heterogeneity buffers the impact of extreme winter weather on wildlife

#load packages
library(ggplot2)
library(dplyr)
library(glmmTMB)
library(corrplot)
library(ggeffects)
library(datawizard)

#read data and create a column with aspect transformed so it goes from 1 (0 deg) to -1 (180 deg)
pitDat <- read.csv("DenaliPits.csv") %>% 
  mutate(Aspect = cos((pi/180)*AspectDeg)) %>%
  mutate(Veg = factor(Veg))
covar<-pitDat[,10:17]   #continuous covariates

CorCovar<-cor(covar)  #correlation matrix of covariates
##Plot of correlations among continuous covariates (Supplementary Fig. 4)
corrplot(CorCovar, type = "upper", addCoef.col = 'black', 
         tl.col = "black", diag = FALSE, tl.cex = .9, number.cex = .9)

#Model of factors affecting ice thickness
#fit Tweedie distribution with glmmTMB, best fit data (zero-inflated continuous)
#had identical AIC as neg binomial but with lower dispersion (3 instead of 4)

#with continuous variables scaled
GlobalTMBScaled <- glmmTMB(totalThickness ~ scale(Slope) + scale(Elevation) + scale(Aspect) + scale(TPI) + scale(TRI) + scale(Day) + scale(Rain) + scale(SnowDepth) + Veg + (1 | PairID), family = tweedie(), data  = pitDat)
summary(GlobalTMBScaled)
##These results are reported in Supplementary Table 1
##same 3 factors are significant no matter which distribution: Day, SnowDepth, Veg

#global tweedie glmm, unscaled
GlobalTMB <- glmmTMB(totalThickness ~ Slope + Elevation + Aspect + 
                TPI + TRI + Day + Rain + SnowDepth + Veg + (1 | PairID), family = tweedie(), data  = pitDat)
summary(GlobalTMB)
#same AIC and significance as scaled, just diff coef vals. Used unscaled to make
#predictions so values are easier to visualize and interpret

##Best model
BestTMB <- glmmTMB(totalThickness ~ Day + Veg + SnowDepth + (1 | PairID), family = tweedie(), data  = pitDat)
summary(BestTMB)

##boxplot of veg vs ice
p <- ggplot(pitDat, aes(x=Veg, y=totalThickness, fill=Veg)) + 
  geom_boxplot(width=0.5, notch = TRUE)
p + labs(x="Vegetation type", y = "Total Ice Thickness (cm)")+
  scale_fill_brewer(palette="Greens")+
  scale_x_discrete(limits=c("Tundra", "Shrub", "Forest"))+
  theme_classic()+theme(legend.position="none")

# generate predictions from best glmmTMB model

# ggeffects, generates prediction while averaging other variables
## for Days since storm
SimDay<-predict_response(BestTMB, terms = c("Day","Veg")) 

##Days since storm vs ice by veg type, model predictions and raw data plotted (Fig. 3b)
DayPlot<-plot(SimDay, show_data = TRUE, show_title = FALSE, show_legend = TRUE, jitter = 0.1)+
  labs(x = "Days Since Storm",y="Total Ice Thickness (cm)")+
  theme_classic()+
  theme(legend.position = c(0.5,0.9),legend.direction = "horizontal", 
        legend.position = "top", legend.text = element_text(size = 10), 
        legend.title=element_blank())+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), 
        plot.margin = margin(10, 20, 10, 10))

DayPlot

##ggeffects simulation for snow depth
SimDepth<-predict_response(BestTMB, c("SnowDepth", "Veg")) 

##SnowDepth vs ice by veg type, model predictions and raw data plotted 
##Note this plot predicts beyond the range of observed values in forest and shrub

DepthPlot1 <- plot(SimDepth, show_data = TRUE, show_title = FALSE, show_legend = FALSE, 
                   jitter = 0.1)+
  labs(x = "Snow Depth (cm)",y="Total Ice Thickness (cm)")+
  theme_classic()+ 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), 
        plot.margin = margin(10, 20, 10, 10))

DepthPlot1

##Alternative plot of SnowDepth that limits predictions to observed values in each veg class
##Used to make Fig 3b

# find min max of snowdepth
minMax_snowdepth <- pitDat %>% 
  group_by(Veg) %>%
  summarise(minSnowDepth = min(SnowDepth), maxSnowDepth = max(SnowDepth))

# join min max with the predicted data, and then omit obs higher or lower
SimDepth1 <- data.frame(SimDepth) %>% 
  rename(Veg = group, SnowDepth = x) %>%
  left_join(minMax_snowdepth) %>%
  filter(SnowDepth >= minSnowDepth, SnowDepth <= maxSnowDepth)

##SnowDepth vs ice by veg type, model predictions and raw data plotted (Fig. 3c)
DepthPlot2 <- ggplot(SimDepth1, aes(SnowDepth, predicted, colour = Veg, fill = Veg)) +
  scale_fill_manual(values=c("red3", "steelblue3", "green4"))+
  scale_color_manual(values=c("red3", "steelblue3", "green4"))+
  scale_y_continuous(limits = c(0, 40))+
  geom_line(linewidth = 0.75, show.legend = FALSE) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, alpha = 0.15, 
              colour = NA) +
  geom_point(data = pitDat, aes(y = totalThickness), alpha = 0.2, show.legend = FALSE) +
  labs(x = "Snow Depth (cm)",y="Total Ice Thickness (cm)", alpha = 0.5)+
  theme_classic()+ 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), 
        plot.margin = margin(10, 20, 10, 10))

DepthPlot2
