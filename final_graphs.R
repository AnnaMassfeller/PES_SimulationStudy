#file for final graphs used in publication
#Graphs clean 06.12
library(ggplot2)
library(dplyr)
#library(rlang)
library(ggh4x)
library(tidyr)
library(ggpubr)
library(RColorBrewer)
library(stringr)
library(tidyverse)
library(ggbreak)
library(scales)
display.brewer.pal(n = 9,name ="Spectral")


#first merge all datasets
#in order to be able to merge the datasets we artifically create column for 
#PV values for outcome beased-schemes by simply dublicating the test.batch.opt df


test.batch.opt_AM_robot_1<-test.batch.opt_AM_robot%>%mutate(Pv = 0.2)
test.batch.opt_AM_robot_2<-test.batch.opt_AM_robot%>%mutate(Pv = 0.4)
test.batch.opt_AM_robot_3<-test.batch.opt_AM_robot%>%mutate(Pv = 0.6)
test.batch.opt_AM_robot_4<-test.batch.opt_AM_robot%>%mutate(Pv = 0.8)
test.batch.opt_AM_robot_5<-test.batch.opt_AM_robot%>%mutate(Pv = 1)
test.batch.opt_AM_robot <- rbind(test.batch.opt_AM_robot_1,test.batch.opt_AM_robot_2,test.batch.opt_AM_robot_3,test.batch.opt_AM_robot_4,test.batch.opt_AM_robot_5)

#merge outcome and action-based
test.batch.opt_AM_robot<-test.batch.opt_AM_robot%>%mutate(new_col="O")
test.batch.act_AM_robot<-test.batch.act_AM_robot%>%mutate(new_col="A")
test.batch.opt_AM_robot<-test.batch.opt_AM_robot %>% rename("d"="db")
test.batch.act_AM_robot<-test.batch.act_AM_robot %>% rename("d"="dc")
test.batch.all<-rbind(test.batch.opt_AM_robot,test.batch.act_AM_robot)
test.batch.all<-test.batch.all %>% rename("type"="new_col")

#remove all observations where R is Na, NaN or Inf as this is due to some problems earlier i.e. I is <= 0
test.batch.all<-test.batch.all[is.finite(test.batch.all$R), ]


#now creater raster plots
#therefore convert respective vars into factors
test.batch.all$p0 <- as.factor(test.batch.all$p0)
test.batch.all$a <- as.factor(test.batch.all$a)
test.batch.all$d <- as.factor(test.batch.all$d)
test.batch.all$x <- as.factor(test.batch.all$x)
test.batch.all$Cl <- as.factor(test.batch.all$Cl)
#test.batch.all$robot <- as.factor(test.batch.all$robot)
test.batch.all$type <- as.factor(test.batch.all$type)
test.batch.all$Pv <- as.factor(test.batch.all$Pv)
test.batch.all$Ch <- as.factor(test.batch.all$Ch)

# New facet label names 
type.labs <- c("action-based", "results-based")
names(type.labs) <- c("A", "O")

robot.labs <- c("no robot", "robot")
names(robot.labs) <- c("0", "1")

a.labs <- c("a = 2/3", "a = 1","a = 10")
names(a.labs) <- c("0.667", "1", "10") #0.666666666666667

d.labs <- c("d = 0.1","d = 0.2","d = 0.3", "d = 0.4","d = 0.5","d = 0.6","d = 0.7", "d = 0.8","d = 0.9","d = 1" )
names(d.labs) <- c("0.1", "0.2","0.3", "0.4","0.5","0.6", "0.7","0.8","0.9", "1")

p0.labs <- c("p0 = 0", "p0 = 0.5", "p0 = 1")
names(p0.labs)<- c("0", "0.5", "1")

CL.labs <- c("CL = 25","CL = 50", "CL = 100", "CL = 200")
names(CL.labs)<- c("25","50", "100", "200")

Pv.labs <- c("Pv = 0.2", "Pv = 0.6","Pv = 1")
names(Pv.labs) <- c("0.2", "0.6", "1")

Ch.labs <- c("Ch = 2", "Ch = 10", "Ch = 20")
names(Ch.labs)<-c("2","10", "20")

#create var for maximum gain achievable
test.batch.all$maxGain <- (test.batch.all$G/(1-as.numeric(as.character(test.batch.all$p0))))

#caculate efficiency on our own for check
test.batch.all$efficiency <- test.batch.all$G/ test.batch.all$E #ok, is same

#Scale all relevant outcome-vars between 0 & 1
scale_values <- function(x){(x-min(x))/(max(x)-min(x))}
test.batch.all$E_scaled <- scale_values(test.batch.all$E)
test.batch.all$G_scaled <- scale_values(test.batch.all$G)
test.batch.all$maxGain_scaled <- scale_values(test.batch.all$maxGain)
test.batch.all$R_scaled <- scale_values(test.batch.all$R)
#cut G at 0 
test.batch.all$G_cut0 <- ifelse(test.batch.all$G >=0, test.batch.all$G,0)

#create dfs for comparison
df.compare_type_all <- test.batch.all%>% distinct() %>% pivot_wider(names_from = "type", 
                                                                    values_from = c("B", "E","G", "R", "L", "I","maxGain","G_cut0", "efficiency", "G_scaled", "E_scaled", "R_scaled", "maxGain_scaled"))

#exclude observations where Cl or Ch are 0 before scaling
df.compare_type_all <- df.compare_type_all %>% filter(Ch != 0, Cl !=0, x == 10)

#scale between -1 and 1
library(scales)
#A - O --> if delta >0, Abs are preferred, <0, RBS are preferred

df.compare_type_all$delta_G <- df.compare_type_all$G_A - df.compare_type_all$G_O
df.compare_type_all$delta_G_cut0 <- df.compare_type_all$G_cut0_A - df.compare_type_all$G_cut0_O
df.compare_type_all$delta_E <- df.compare_type_all$E_A - df.compare_type_all$E_O
df.compare_type_all$delta_R <- df.compare_type_all$R_A - df.compare_type_all$R_O
df.compare_type_all$delta_maxGain <- df.compare_type_all$maxGain_A - df.compare_type_all$maxGain_O
df.compare_type_all$delta_I <- df.compare_type_all$I_A - df.compare_type_all$I_O
df.compare_type_all$delta_L <- df.compare_type_all$L_A - df.compare_type_all$L_O
df.compare_type_all$delta_B <- df.compare_type_all$B_A - df.compare_type_all$B_O

df.compare_type_all$delta_G_scaled<- ifelse(df.compare_type_all$delta_G < 0 , -df.compare_type_all$delta_G/min(df.compare_type_all$delta_G, na.rm = T), df.compare_type_all$delta_G/max(df.compare_type_all$delta_G, na.rm = T))
df.compare_type_all$delta_E_scaled<- ifelse(df.compare_type_all$delta_E < 0 , -df.compare_type_all$delta_E/min(df.compare_type_all$delta_E, na.rm = T), df.compare_type_all$delta_E/max(df.compare_type_all$delta_E, na.rm = T))
df.compare_type_all$delta_R_scaled<- ifelse(df.compare_type_all$delta_R < 0 , -df.compare_type_all$delta_R/min(df.compare_type_all$delta_R, na.rm = T), df.compare_type_all$delta_R/max(df.compare_type_all$delta_R, na.rm = T))
df.compare_type_all$delta_maxGain_scaled<- ifelse(df.compare_type_all$delta_maxGain < 0 , -df.compare_type_all$delta_maxGain/min(df.compare_type_all$delta_maxGain, na.rm = T), df.compare_type_all$delta_maxGain/max(df.compare_type_all$delta_maxGain, na.rm = T))

df.compare_type_all <- df.compare_type_all %>% filter(!(is.na(delta_R_scaled)))

#create artifical facetting variable
df.compare_type_all$Comp <- 1
df.compare_type_all$Comp<- as.factor(df.compare_type_all$Comp)

Comp.labs <- c("Difference between ABS and RBS")
names(Comp.labs) <- c("1")



###create plots for publication

#Figure 1
Fig1_Biodiv_aP0L_ABSRBS <- test.batch.all %>% filter(Ch != 0,x!= 0, Cl != 0, 
                                                # d %in% c(0.1, 0.5,0.9),
                                                Pv == 1,
                                                Cl ==100,
                                                d == 0.9,
                                                Ch ==10,
                                                x == 10,
                                                a %in% c(0.667,1,10)
)%>% 
 ggplot()+
 geom_point(size=3,aes(y=L, x = interaction(p0,sep = "!"), colour = B, shape = type))+
 scale_color_gradientn(name = "Probability biodiveristy occurs\ngiven scheme participation\n(PL)",
                       breaks=c(0.4,0.6,0.8,1),
                       colors = (hcl.colors(7, palette = "Spectral")))+
 scale_shape_manual(values = c(8, 19), name = "Scheme type",labels = c("ABS", "RBS"))+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 scale_y_continuous(breaks = c(1,2))+
 xlab("Probability of biodiversity occuring on patch (P0)")+
 ylab("Level of action (L)")+
 theme_bw()+
 facet_nested(~a, labeller = labeller(type = type.labs, 
                                      robot = robot.labs, a = a.labs, d = d.labs, 
                                      Pv = Pv.labs, 
                                      Cl = CL.labs
                                      #Ch = Ch.labs
 ))+
 theme(
  text = element_text(size = 10),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 12),
  legend.text = element_text(size = 12),
  legend.title = element_text(size = 12),
  strip.text = element_text(size = 10)
 )

Fig1_Biodiv_aP0L_ABSRBS
ggsave("Fig1_Biodiv_aP0L_ABSRBS.png",device='png', dpi=1000, width = 22, height = 15, units = "cm")



#Figure 2
# Combine the data for both ABS and RBS
BiodivGain_combined <- test.batch.all %>%
 filter(type %in% c("A", "O"), Ch != 0, x != 0, Cl != 0, 
        d == 0.9, Pv == 0.6, Cl == 100, Ch == 10, x == 10, a != 0.666666666666667)

# Plot with faceting based on the type
BiodivGain_combined_plot <- ggplot(BiodivGain_combined) +
 geom_tile(aes(y = a, x = interaction(p0, sep = "!"), fill = G_cut0)) +
 scale_fill_gradientn(name = "Biodiversity gain",
                      breaks = c(0, 0.5, 0.9),
                      colours = hcl.colors(7, palette = "Spectral")) +
 scale_x_discrete(breaks = c("0.2", "0.5", "0.8")) +
 scale_y_discrete(breaks = c("1", "5", "10")) +
 xlab("") +
 ylab("Level of sensitivity to action (a)") +
 theme_bw() +
 theme(legend.position = "right") +
 facet_grid(type~., labeller = labeller(type = type.labs)) + # Facet based on 'type'
 theme(
  text = element_text(size = 16),          # Overall text size
  axis.title = element_text(size = 18),    # Axis titles
  axis.text = element_text(size = 14),     # Axis labels
  legend.text = element_text(size = 14),   # Legend text
  legend.title = element_text(size = 16),  # Legend title
  strip.text = element_text(size = 16)     # Facet labels
 )
BiodivGain_combined_plot

#add comparison plot
Comp_BiodivGain_aP0L_ABSRBS<- df.compare_type_all %>% filter (d == 0.9, Ch == 10, Cl == 100, Pv ==0.6, a != 0.333333333333333
) %>% 
 ggplot()+ 
 geom_raster(aes(y=a, x = p0, fill = delta_G_cut0))+
 scale_fill_gradient2(na.value = "red",low = '#D53E4F', high = '#3288BD',mid = "#FFFFBF",
                      name = "Difference in\nBiodiversity gain", midpoint = 0
                      ,breaks=c(0.04,0,- 0.1),
                      #limits=c(1, - 1),
                      labels=c("ABS higher","no difference","RBS higher"))+
 ylab("Level of sensitivity to action (a)") +
 xlab("Probability of biodiversity occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.2", "0.5", "0.8"))+
 scale_y_discrete(breaks = c("1", "5", "10"))+
 #scale_y_break(breaks = c(5,8))+
 theme_bw()+
 theme(legend.position = "none")+
 facet_nested(Comp~., labeller = labeller(Comp = Comp.labs))+
 theme(
  text = element_text(size = 16),          # Overall text size
  axis.title = element_text(size = 18),    # Axis titles
  axis.text = element_text(size = 14),     # Axis labels
  legend.text = element_text(size = 14),   # Legend text
  legend.title = element_text(size = 16),  # Legend title
  strip.text = element_text(size = 16)     # Facet labels
 )

# Save the combined plot
ggsave("BiodivGain_combined_ABS_RBS.png", plot = BiodivGain_combined_plot, device = 'png', dpi = 800, width = 10, height = 6, units = "cm")



#for better readability in efficiency plots (Figure 3):
# Convert 'a' into a factor and reverse its levels
df.compare_type_all <- df.compare_type_all %>%
 mutate(a = factor(a, levels = c(10, 1, 0.666666666666667)))  # Reverse levels

a.labs.reverse <- c("a = 10", "a = 1", "a = 2/3")
names(a.labs.reverse) <- c("10", "1", "0.666666666666667")




#Figure 3a
Fig3a_Eff_a_d_Ch <- df.compare_type_all %>%
 filter(Cl == 100, Pv == 0.6, d %in% c(0.1, 0.5, 0.9), a %in% c(0.666666666666667, 1, 10)) %>%
 ggplot() +
 geom_raster(aes(y = Ch, x = p0, fill = delta_R)) +
 scale_fill_gradient2(
  na.value = "red",
  low = '#D53E4F',
  high = '#3288BD',
  mid = "#FFFFBF",
  name = "Difference in Efficiency",
  midpoint = 0,
  breaks = c(0.0002, 0, -0.0002),
  labels = c("ABS higher", "no difference", "RBS higher"))+
   ylab("Cost of agency monitoring/h (Ch)") +
   xlab("Probability of biodiversity occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.2", "0.5", "0.8")) +
 scale_y_discrete(breaks = c("4", "8", "12", "16", "20")) +
 theme(legend.position = "right") +
 theme_bw() +
 facet_grid(a ~ d, labeller = labeller(a = a.labs.reverse, d = d.labs, Pv = Pv.labs)) +  # No need to reverse 'a' here
 theme(
  text = element_text(size = 10),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 12),
  legend.text = element_text(size = 12),
  legend.title = element_text(size = 12),
  strip.text = element_text(size = 10)
 )
Fig3a_Eff_a_d_Ch
ggsave("Fig3ac_Eff_a_d_Ch_en.png",device='png', dpi=1000, width = 22, height = 15, units = "cm")

#Figure 3b
Fig3b_Eff_a_d_Pv <-df.compare_type_all %>% filter (Ch == 10, Cl == 100,
                                             d %in% c(0.1, 0.5, 0.9), a %in% c(0.666666666666667,1,10)) %>% 
 ggplot()+ 
 geom_raster(aes(y=Pv, x = p0, fill = delta_R))+
 scale_fill_gradient2(na.value = "red",low = '#D53E4F', high = '#3288BD',mid = "#FFFFBF",
                      name = "Difference in Efficiency", midpoint = 0
                      ,breaks=c(0.0002,0,- 0.0002),
                      #limits=c(1, - 1),
                      labels=c("ABS higher","no difference","RBS higher"))+
 ylab("Probability farms are visited for monitoring non-compliance (Pv)") +
 xlab("Probability of biodiversity occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 theme_bw()+
 facet_grid(a ~ d,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))+
 theme(
  text = element_text(size = 10),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 12),
  legend.text = element_text(size = 12),
  legend.title = element_text(size = 12),
  strip.text = element_text(size = 10)
 )
Fig3b_Eff_a_d_Pv
ggsave("Fig3b_Eff_a_d_Pv.png",device='png', dpi=1000, width = 22, height = 15, units = "cm")

#Figure 3c
Fig3c_Eff_a_d_Cl<-df.compare_type_all %>% filter (Ch == 10, Pv == 0.6,
                                            d %in% c(0.1, 0.5, 0.9),a %in% c(0.666666666666667,1,10)) %>% 
 ggplot()+ 
 geom_raster(aes(y=Cl, x = p0, fill = delta_R))+
 scale_fill_gradient2(na.value = "red",low = '#D53E4F', high = '#3288BD',mid = "#FFFFBF",
                      name = "Difference in Efficiency", midpoint = 0
                      ,breaks=c(0.0009,0,- 0.0009),
                      #limits=c(1, - 1),
                      labels=c("ABS higher","no difference","RBS higher"))+
 ylab("Cost of setting action (CL)") +
 xlab("Probability of biodiversity occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 theme_bw()+
 facet_grid(a ~ d,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))+
 theme(
  text = element_text(size = 10),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 12),
  legend.text = element_text(size = 12),
  legend.title = element_text(size = 12),
  strip.text = element_text(size = 10)
 )
 
Fig3c_Eff_a_d_Cl
ggsave("Fig3c_Eff_a_d_Cl.png",device='png', dpi=1000, width = 22, height = 15, units = "cm")


#sensitivty analysis fpr different levels of x
#1.always p0 and x, then facet by other parameters
#####
#1.a and d
df.compare_type_all %>% filter (Pv == 0.6, Cl == 100, Ch == 10,
                                d %in% c(0.1, 0.5, 0.9)) %>% 
 ggplot()+ 
 geom_raster(aes(y=x, x = p0, fill = delta_R_scaled))+
 scale_fill_gradient2(na.value = "red",low = '#0072B2', high = '#F0E442',mid = "white",
                      name = "Difference in Efficiency", midpoint = 0
                      ,breaks=c(0.1,0,- 0.1),
                      #limits=c(1, - 1),
                      labels=c("ABS higher","no difference","RBS higher"))+
 ylab("Level of x") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(d~a,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))


#2.Ch and Cl
df.compare_type_all %>% filter (Cl !=0, Ch != 0, Pv == 0.6, a == 1, d == 0.5) %>% 
 ggplot()+ 
 geom_raster(aes(y=x, x = p0, fill = delta_R_scaled))+
 scale_fill_gradient2(na.value = "red",low = '#0072B2', high = '#F0E442',mid = "white",
                      name = "Difference in Efficiency", midpoint = 0
                      ,breaks=c(0.1,0,- 0.1),
                      #limits=c(1, - 1),
                      labels=c("ABS higher","no difference","RBS higher"))+
 ylab("Level of d") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(Ch ~ Cl,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))

#3. Pv and a
df.compare_type_all %>% filter (Ch == 10, Cl == 100, a == 1) %>% 
 ggplot()+ 
 geom_raster(aes(y=x, x = p0, fill = delta_R_scaled))+
 scale_fill_gradient2(na.value = "red",low = '#0072B2', high = '#F0E442',mid = "white",
                      name = "Difference in Efficiency", midpoint = 0
                      ,breaks=c(0.1,0,- 0.1),
                      #limits=c(1, - 1),
                      labels=c("ABS higher","no difference","RBS higher"))+
 ylab("Level of x") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(Pv ~ d,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))





