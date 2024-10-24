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
#cols <- (YlGn(8)[-8])

#cols <- c("#000000","#004949","#009292","#ff6db6","#ffb6db",
#"#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
#"#920000","#924900","#db6d00","#24ff24","#ffff6d")

#first merge all datasets
#in order to be able to merge the datasets we artifically create column for 
#PV values for outcome beased-schemes by simply dublicating the test.batch.opt df


test.batch.opt_AM_robot_1<-test.batch.opt_AM_robot%>%mutate(Pv = 0.2)
test.batch.opt_AM_robot_2<-test.batch.opt_AM_robot%>%mutate(Pv = 0.4)
test.batch.opt_AM_robot_3<-test.batch.opt_AM_robot%>%mutate(Pv = 0.6)
test.batch.opt_AM_robot_4<-test.batch.opt_AM_robot%>%mutate(Pv = 0.8)
test.batch.opt_AM_robot_5<-test.batch.opt_AM_robot%>%mutate(Pv = 1)
test.batch.opt_AM_robot <- rbind(test.batch.opt_AM_robot_1,test.batch.opt_AM_robot_2,test.batch.opt_AM_robot_3,test.batch.opt_AM_robot_4,test.batch.opt_AM_robot_5)

#we do not need the differentiation in robot/no robot anmyore as we simply simlaute ranges


#action-based
#test.batch.act_AM<-test.batch.act_AM%>%mutate(robot=0)
#test.batch.act_AM<-test.batch.act_AM%>%mutate(Cl=100)
#test.batch.act_AM<-test.batch.act_AM%>%mutate(Ch=10)
#test.batch.act_AM<-test.batch.act_AM%>%mutate(Pv=0.6)
#test.batch.act_AM_robot<-test.batch.act_AM_robot%>%mutate(robot=1)

#test.batch.act_all<-rbind(test.batch.act_AM, test.batch.act_AM_robot)

#outcome-based
#test.batch.opt_AM<-test.batch.opt_AM%>%mutate(robot=0)
#test.batch.opt_AM<-test.batch.opt_AM%>%mutate(Cl=100)
#test.batch.opt_AM<-test.batch.opt_AM%>%mutate(Ch=10)
#test.batch.opt_AM<-test.batch.opt_AM%>%mutate(Pv=0.6)
#test.batch.opt_AM_robot<-test.batch.opt_AM_robot%>%mutate(robot=1)

#test.batch.opt_all<-rbind(test.batch.opt_AM, test.batch.opt_AM_robot)


#merge outcome and action-based
test.batch.opt_AM_robot<-test.batch.opt_AM_robot%>%mutate(new_col="O")
test.batch.act_AM_robot<-test.batch.act_AM_robot%>%mutate(new_col="A")
test.batch.opt_AM_robot<-test.batch.opt_AM_robot %>% rename("d"="db")
test.batch.act_AM_robot<-test.batch.act_AM_robot %>% rename("d"="dc")
test.batch.all<-rbind(test.batch.opt_AM_robot,test.batch.act_AM_robot)
test.batch.all<-test.batch.all %>% rename("type"="new_col")


#merge outcome and action-based
#test.batch.opt_AM_robot<-test.batch.opt_AM_robot%>%mutate(new_col="O")
#test.batch.act_AM_robot<-test.batch.act_AM_robot%>%mutate(new_col="A")
#test.batch.opt_AM_robot<-test.batch.opt_AM_robot %>% rename("d"="db")
#test.batch.act_AM_robot<-test.batch.act_AM_robot %>% rename("d"="dc")
#test.batch.opt_AM_robot<-test.batch.opt_AM_robot%>%mutate(Pv=0.56)
#test.batch.all<-rbind(test.batch.opt_AM_robot,test.batch.act_AM_robot)
#test.batch.all<-test.batch.all %>% rename("type"="new_col")

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
names(a.labs) <- c("0.666666666666667", "1", "10") #0.666666666666667

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
#test.batch.all[is.na(test.batch.all$maxGain),]$maxGain <- 0

#let's focus on the case we aim to plot
#test.batch.all_full <- test.batch.all
#test.batch.all <- test.batch.all_full
#test.batch.all <-test.batch.all %>% filter(x == 10,
 #                                          Pv %in% c(0.2, 0.6, 1),
  #                                         Cl %in% c(50,100,200),
   #                                        Ch %in% c(2,10,20))

#Scale all relevant outcome-vars between 0 & 1
scale_values <- function(x){(x-min(x))/(max(x)-min(x))}
test.batch.all$E_scaled <- scale_values(test.batch.all$E)
test.batch.all$G_scaled <- scale_values(test.batch.all$G)
test.batch.all$maxGain_scaled <- scale_values(test.batch.all$maxGain)
test.batch.all$R_scaled <- scale_values(test.batch.all$R)
#cut G at 0 
test.batch.all$G_cut0 <- ifelse(test.batch.all$G >=0, test.batch.all$G,0)

#what if we define efficiency as maxgain per x?
#test.batch.all$efficiency_Mgx <- as.numeric(test.batch.all$maxGain)/as.numeric(test.batch.all$x)
#summary(test.batch.all$efficiency_Mgx)
#scale it
#test.batch.all$efficiency_Mgx_scaled <-scale_values(test.batch.all$efficiency_Mgx)
#summary(test.batch.all$efficiency_Mgx_scaled)
#create plots with absolute value


#final plots for absolute values
#max gain is mainly a fucntion of a and p0
#ch, cl and Pv have no influence on max gain, onl yon expenditures and ten efficiency
#y-axis = a
maxGain_allParam_ABSRBS <- test.batch.all %>% filter(Ch != 0,x!= 0, Cl != 0, 
                         # d %in% c(0.1, 0.5,0.9),
                          Pv %in% c(0.2, 0.6, 1),
                          Cl %in% c(50,100,200),
                          Ch %in% c(2,10,20),
                         a %in% c(0.667,1,10)
                          ) %>% 
 ggplot()+
 geom_tile(aes(y=d, x = interaction(p0,sep = "!"), fill = B))+
  scale_fill_gradientn(name = "Biodiversity Gain",
                       breaks=c(0, 0.25, 0.5, 0.75,1),
                       colours = (hcl.colors(7, palette = "Spectral")))+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 scale_y_discrete(breaks = c("0.1", "0.5", "0.9"))+
 xlab("Probability of benefit occuring on patch (P0)")+
 ylab("Level of d")+
 facet_nested(type+Cl + Ch ~a+Pv, labeller = labeller(type = type.labs, 
                                               robot = robot.labs, a = a.labs, d = d.labs, 
                                               Pv = Pv.labs, 
                                               Cl = CL.labs
                                               #Ch = Ch.labs
                                               ))

Biodiv_aP0L_ABSRBS <- test.batch.all %>% filter(Ch != 0,x!= 0, Cl != 0, 
                                                     # d %in% c(0.1, 0.5,0.9),
                                                     Pv == 1,
                                                     Cl ==100,
                                                     d == 0.9,
                                                     Ch ==10#,
                                                    # a %in% c(0.666666666666667,1,10)
                                                    )%>% 
 ggplot()+
 geom_tile(aes(y=a, x = interaction(p0,sep = "!"), fill = B))+
 scale_fill_gradientn(name = "Prob. Biodiversity occurs\ngiven scheme participation (PL)",
                      breaks=c(0.1, 0.5,1),
                      colours = (hcl.colors(7, palette = "Spectral")))+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 scale_y_discrete(breaks = c("1", "5", "10"))+
 xlab("Probability of biodiversity occuring on patch (P0)")+
 ylab("Level of sensitivity to action (a)")+
 facet_nested(~type, labeller = labeller(type = type.labs, 
                                                      robot = robot.labs, a = a.labs, d = d.labs, 
                                                      Pv = Pv.labs, 
                                                      Cl = CL.labs
                                                      #Ch = Ch.labs
 ))


#try to plot Pl in dependence of a, P0 and L for both scheme types

Biodiv_aP0L_ABSRBS <- test.batch.all %>% filter(Ch != 0,x!= 0, Cl != 0, 
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
 ))

#try to plot Pl in dependence of a, P0 and L for both scheme types

Biodiv_aP0L_ABSRBS <- test.batch.all %>% filter(Ch != 0,x!= 0, Cl != 0, 
                                                # d %in% c(0.1, 0.5,0.9),
                                                Pv == 1,
                                                Cl ==100,
                                                d == 0.9,
                                                Ch ==10,
                                                x == 10,
                                                a %in% c(0.666666666666667,1,10)
)%>% 
 ggplot()+
 geom_jitter(aes(y=L, x = interaction(p0,sep = "!"), colour = B))+
 scale_color_gradientn(name = "Probability biodiveristy occurs\ngiven scheme participation\n(PL)",
                       breaks=c(0, 0.5, 1),
                       colors = (hcl.colors(7, palette = "Spectral")))+
# scale_shape_manual(values = c(4, 19), name = "Scheme type",labels = c("ABS", "RBS"))+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 scale_y_continuous(breaks = c(1,3,5))+
 xlab("Probability of biodiversity occuring on patch (P0)")+
 ylab("Level of action (L)")+
 theme_bw()+
 facet_nested(type~a, labeller = labeller(type = type.labs, 
                                      robot = robot.labs, a = a.labs, d = d.labs, 
                                      Pv = Pv.labs, 
                                      Cl = CL.labs
                                      #Ch = Ch.labs
 ))


Level_aP0L_ABSRBS <- test.batch.all %>% filter(Ch != 0,x!= 0, Cl != 0, 
                                                    # d %in% c(0.1, 0.5,0.9),
                                                    Pv == 1,
                                                    Cl ==100,
                                                    Ch ==10#,
                                                    # a %in% c(0.666666666666667,1,10)
)%>% 
 ggplot()+
 geom_tile(aes(y=a, x = interaction(p0,sep = "!"), fill = L))+
 scale_fill_gradientn(name = "Level of action",
                      breaks=c(0,1,5),
                      colours = (hcl.colors(7, palette = "Spectral")))+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 scale_y_discrete(breaks = c("1", "5", "10"))+
 xlab("Probability of biodiversity occuring on patch (P0)")+
 ylab("Level of sensitivity to action (a)")+
 facet_nested(~type, labeller = labeller(type = type.labs, 
                                         robot = robot.labs, a = a.labs, d = d.labs, 
                                         Pv = Pv.labs, 
                                         Cl = CL.labs
                                         #Ch = Ch.labs
 ))

BiodivGain_aP0L_ABSRBS <- test.batch.all %>% filter(Ch != 0,x!= 0, Cl != 0, 
                                               d == 0.9,
                                               Pv == 1,
                                               Cl ==100,
                                               Ch ==10,
                                               x == 10,
                                               a != 0.666666666666667
)%>% 
 ggplot()+
 geom_tile(aes(y=a, x = interaction(p0,sep = "!"), fill = G_cut0))+
 scale_fill_gradientn(name = "Biodiversity gain",
                      breaks=c(0,0.5,0.9),
                      colours = (hcl.colors(7, palette = "Spectral")))+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 scale_y_discrete(breaks = c("1", "5", "10"))+
 xlab("Probability of biodiversity occuring on patch (P0)")+
 ylab("Level of sensitivity to action (a)")+
 theme_bw()+
 theme(legend.position = "right")+
 facet_nested(.~type, labeller = labeller(type = type.labs, 
                                         robot = robot.labs, a = a.labs, d = d.labs, 
                                         Pv = Pv.labs, 
                                         Cl = CL.labs
                                         #Ch = Ch.labs
 ))

#only by scheme for comparison plot
BiodivGain_aP0L_ABS <- test.batch.all %>% filter(type == "A",Ch != 0,x!= 0, Cl != 0, 
                                                    d == 0.9,
                                                    Pv == 0.6,
                                                    Cl ==100,
                                                    Ch ==10,
                                                    x == 10,
                                                    a != 0.666666666666667
)%>% 
 ggplot()+
 geom_tile(aes(y=a, x = interaction(p0,sep = "!"), fill = G_cut0))+
 scale_fill_gradientn(name = "Biodiversity gain",
                      breaks=c(0,0.5,0.9),
                      colours = (hcl.colors(7, palette = "Spectral")))+
 scale_x_discrete(breaks = c("0.2", "0.5", "0.8"))+
 scale_y_discrete(breaks = c("1", "5", "10"))+
 xlab("")+
 ylab("Level of sensitivity to action (a)")+
 theme_bw()+
 theme(legend.position = "none")+
 facet_nested(type~., labeller = labeller(type = type.labs))+
 theme(
  text = element_text(size = 16),          # Overall text size
  axis.title = element_text(size = 18),    # Axis titles
  axis.text = element_text(size = 14),     # Axis labels
  legend.text = element_text(size = 14),   # Legend text
  legend.title = element_text(size = 16),  # Legend title
  strip.text = element_text(size = 16)     # Facet labels
 )
                                          
ggsave("BiodivGain_aP0L_ABS.png",device='png', dpi=800, width = 10, height = 6, units = "cm")

BiodivGain_aP0L_RBS <- test.batch.all %>% filter(type == "O",Ch != 0,x!= 0, Cl != 0, 
                                                    d == 0.9,
                                                    Pv == 0.6,
                                                    Cl ==100,
                                                    Ch ==10,
                                                    x == 10,
                                                    a != 0.666666666666667
)%>% 
 ggplot()+
 geom_tile(aes(y=a, x = interaction(p0,sep = "!"), fill = G_cut0))+
 scale_fill_gradientn(name = "Biodiversity gain",
                      breaks=c(0,0.5,0.9),
                      colours = (hcl.colors(7, palette = "Spectral")))+
 scale_x_discrete(breaks = c("0.2", "0.5", "0.8"))+
 scale_y_discrete(breaks = c("1", "5", "10"))+
 xlab("")+
 ylab("Level of sensitivity to action (a)")+
 theme_bw()+
 theme(legend.position = "none")+
facet_nested(type~., labeller = labeller(type = type.labs))+
 theme(
  text = element_text(size = 16),          # Overall text size
  axis.title = element_text(size = 18),    # Axis titles
  axis.text = element_text(size = 14),     # Axis labels
  legend.text = element_text(size = 14),   # Legend text
  legend.title = element_text(size = 16),  # Legend title
  strip.text = element_text(size = 16)     # Facet labels
 )

ggsave("BiodivGain_aP0L_RBS.png",device='png', dpi=800, width = 10, height = 6, units = "cm")

ggarrange(BiodivGain_aP0L_ABS,BiodivGain_aP0L_RBS, nrow = 2)
ggsave("ABS_RBS.png", device = "png", dpi = 800)

#plot level of action
Level_allParam_ABSRBS <- test.batch.all %>% filter(Ch != 0,x!= 0, Cl != 0, 
                                                     # d %in% c(0.1, 0.5,0.9),
                                                     Pv %in% c(0.2, 0.6, 1),
                                                     Cl %in% c(50,100,200),
                                                     Ch %in% c(2,10,20),
                                                     a %in% c(0.666666666666667,1,10)
) %>% 
 ggplot()+
 geom_tile(aes(y=d, x = interaction(p0,sep = "!"), fill = L))+
 scale_fill_gradientn(name = "Level of action",
                      breaks=c(0.5,1,5),
                      colours = (hcl.colors(7, palette = "Spectral")))+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 scale_y_discrete(breaks = c("0.1", "0.5", "0.9"))+
 xlab("Probability of benefit occuring on patch (P0)")+
 ylab("Level of d")+
 facet_nested(type+Cl + Ch ~a+Pv, labeller = labeller(type = type.labs, 
                                                      robot = robot.labs, a = a.labs, d = d.labs, 
                                                      Pv = Pv.labs, 
                                                      Cl = CL.labs
                                                      #Ch = Ch.labs
 ))

#plot income of action
Income_allParam_ABSRBS <- test.batch.all %>% filter(Ch != 0,x!= 0, Cl != 0, 
                                                   # d %in% c(0.1, 0.5,0.9),
                                                   Pv %in% c(0.2, 0.6, 1),
                                                   Cl %in% c(50,100,200),
                                                   Ch %in% c(2,10,20),
                                                   a %in% c(0.666666666666667,1,10)
) %>% 
 ggplot()+
 geom_tile(aes(y=d, x = interaction(p0,sep = "!"), fill = I))+
 scale_fill_gradientn(name = "Income",
                      #breaks=c(0.5,1,5),
                      colours = (hcl.colors(7, palette = "Spectral")))+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 scale_y_discrete(breaks = c("0.1", "0.5", "0.9"))+
 xlab("Probability of benefit occuring on patch (P0)")+
 ylab("Level of d")+
 facet_nested(type+Cl + Ch ~a+Pv, labeller = labeller(type = type.labs, 
                                                      robot = robot.labs, a = a.labs, d = d.labs, 
                                                      Pv = Pv.labs, 
                                                      Cl = CL.labs
                                                      #Ch = Ch.labs
 ))

Gain_a_ABSRBS <- test.batch.all %>% filter(Ch != 0,x!= 0, Cl != 0, 
                                                     # d %in% c(0.1, 0.5,0.9),
                                                     Pv %in% c(0.2, 0.6, 1),
                                                     Cl %in% c(50,100,200),
                                                     Ch %in% c(2,10,20),
                                                     ) %>% 
 ggplot()+
 geom_tile(aes(y=a, x = interaction(p0,sep = "!"), fill = G_cut0))+
 scale_fill_gradientn(name = "Biodiversity Gain",
                      breaks=c(0, 0.25, 0.5, 0.75,1),
                      colours = (hcl.colors(7, palette = "Spectral")))+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 scale_y_discrete(breaks = c("0.1", "0.5", "0.9"))+
 xlab("Probability of benefit occuring on patch (P0)")+
 ylab("Level of a")+
 facet_nested(~type, labeller = labeller(type = type.labs, 
                                                      robot = robot.labs, a = a.labs, d = d.labs, 
                                                      Pv = Pv.labs, 
                                                      Cl = CL.labs
                                                      #Ch = Ch.labs
 ))


#same for expenditures
Expenditures_allParam_ABSRBS <-test.batch.all %>% filter(x == 10,
                                                         Ch %in% c(2,10,20),
                                                         Cl %in% c(25,100,200)
                                                         ,d %in% c(0.1, 0.5, 0.9),
                                                         Pv %in% c(0.2, 0.6, 1)
                                                         #a %in% c(0.666666666666667,1,10)
) %>% 
 ggplot()+
 geom_tile(aes(y=a, x = interaction(p0,sep = "!"), fill = E))+
 scale_fill_gradientn(name = "Expenditures",colours = rev(hcl.colors(7, palette = "Spectral")))+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 scale_y_discrete(breaks = c("1", "5", "10"))+
 xlab("Probability of biodiversity occuring on patch (P0)")+
 ylab("Level of sensitivity to action (a)")+
 theme_bw()+
 facet_nested(type+Cl~d+Ch +Pv, labeller = labeller(type = type.labs, 
                                               robot = robot.labs, a = a.labs, d = d.labs, 
                                               Pv = Pv.labs, 
                                               Cl = CL.labs,
                                               Ch = Ch.labs
                                               ))


#same for efficiency
Efficiency_allParam_ABSRBS <-test.batch.all %>% filter(Ch != 0,x!= 0, Cl != 0, 
                                                        # d %in% c(0.1, 0.5,0.9),
                                                        Pv %in% c(0.2, 0.6, 1),
                                                        Cl %in% c(50,100,200),
                                                        Ch %in% c(2,10,20),
                                                        a %in% c(0.666666666666667,1,10)
) %>% 
 ggplot()+
 geom_tile(aes(y=d, x = interaction(p0,sep = "!"), fill = efficiency))+
 scale_fill_gradientn(name = "Efficiency",colours = (hcl.colors(7, palette = "Spectral")))+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 scale_y_discrete(breaks = c("0.1", "0.5", "0.9"))+
 xlab("Probability of benefit occuring on patch (P0)")+
 ylab("Level of d")+
 facet_nested(type+Cl + Ch ~ a+Pv, labeller = labeller(type = type.labs, 
                                               robot = robot.labs, a = a.labs, d = d.labs, 
                                               Pv = Pv.labs, 
                                               Cl = CL.labs
                                               #Ch = Ch.labs
                                               ))


maxGain_allParam_ABSRBS #mainly driven by a and type
Expenditures_allParam_ABSRBS # ABS mainly driven by Cl, to a lesser extent by Ch and Pv, RBS lso driven by a, Cl and Ch and slightly d
Efficiency_allParam_ABSRBS # Cl make difference but within CL d, a and p0 are main drivers
Gain_a_ABSRBS
Income_allParam_ABSRBS
Level_allParam_ABSRBS

#create dfs for comparison
#create new df 
df.compare_type_all <- test.batch.all%>% distinct() %>% pivot_wider(names_from = "type", 
                               values_from = c("B", "E","G", "R", "L", "I","maxGain","G_cut0", "efficiency", "G_scaled", "E_scaled", "R_scaled", "maxGain_scaled"))

#exclude observations where Cl or Ch are 0 before scaling
df.compare_type_all <- df.compare_type_all %>% filter(Ch != 0, Cl !=0, x == 10)
#df.compare_type_norobot <- test.batch.all%>% filter(robot == 0) %>% distinct() %>% pivot_wider(names_from = "type", 
 #                                                                                      values_from = c("B", "E", "E_scaled", "G","G_scaled", "R", "L", "I","maxGain", "maxGain_scaled"))

#no create columns for delta E, delta G and delta R for both dfs
#we substract A-O, i.e. when delta is > 0, A is higher, if delta <0, O is higher

#1. robot df


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

#also create binary outcome , i.e. Ah if A is higher (delat is >0) and Rh if R is higher (i.e. delta <0)
df.compare_type_all$delta_G_b <- ifelse(df.compare_type_all$delta_G_scaled >0, "Ah", "Rh")
df.compare_type_all$delta_E_b <- ifelse(df.compare_type_all$delta_E_scaled >0, "Ah", "Rh")
df.compare_type_all$delta_R_b <- ifelse(df.compare_type_all$delta_R_scaled >0, "Ah", "Rh")
df.compare_type_all$delta_maxGain_b <- ifelse(df.compare_type_all$delta_maxGain_scaled >0, "Ah", "Rh")

#2.no-robot-df
#no create columns for delta E, delta G and delta R
#we substract A-O, i.e. when delta is > 0, A is higher, if delta <0, O is higher

#df.compare_type_norobot$delta_G <- df.compare_type_norobot$G_A - df.compare_type_norobot$G_O
#df.compare_type_norobot$delta_E <- df.compare_type_norobot$E_A - df.compare_type_norobot$E_O
#df.compare_type_norobot$delta_R <- df.compare_type_norobot$R_A - df.compare_type_norobot$R_O
#df.compare_type_norobot$delta_maxGain <- df.compare_type_norobot$maxGain_A - df.compare_type_norobot$maxGain_O




#also create binary outcome , i.e. Ah if A is higher (delat is >0) and Rh if R is higher (i.e. delta <0)
#df.compare_type_norobot$delta_G_b <- ifelse(df.compare_type_norobot$delta_G_scaled >0, "Ah", "Rh")
#df.compare_type_norobot$delta_E_b <- ifelse(df.compare_type_norobot$delta_E_scaled >0, "Ah", "Rh")
#df.compare_type_norobot$delta_R_b <- ifelse(df.compare_type_norobot$delta_R_scaled >0, "Ah", "Rh")
#df.compare_type_norobot$delta_maxGain_b <- ifelse(df.compare_type_norobot$delta_maxGain_scaled >0, "Ah", "Rh")



#remove norobot-obsrvations again

#df.compare_type_all <- df.compare_type_all %>% filter(robot == 1, Cl != 0,Ch != 0)
df.compare_type_all <- df.compare_type_all %>% filter(!(is.na(delta_R_scaled)))

#let#s plot some results
# we are interested in the difference in efficiency under different conditions
#therefore we once choose all the parameters related to the costs (Ch, Cl, d)
#and ince the ones related to management and indicators (a, d)
#we always keep p0 as x-axis as this is not changed
myColours <- c("#F0E442","#0072B2","#009E73")

#as binary output doesn't make so much sense when we don't consider x as it looks quite similar/boring for all
#plot similar to above woth d,a,pv,cl and ch on the y-axis

#final plots
#Biodiversity gain-->only varying depending on a
#Level of action
df.compare_type_all %>% filter (d == 0.9, Ch == 10, Cl == 100, Pv ==1
                                ) %>% 
 ggplot()+ 
 geom_raster(aes(y=a, x = p0, fill = delta_L))+
 scale_fill_gradient2(na.value = "red",low = '#D53E4F', high = '#3288BD',mid = "#FFFFBF",
                      name = "Difference in Level of action (L)", midpoint = 0
                      ,breaks=c(0.5,0,-0.5),
                      #limits=c(1, - 1),
                      labels=c("ABS higher","no difference","RBS higher"))+
 ylab("Level of sensitivity to action (a)") +
 xlab("Probability of biodiversity occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 scale_y_discrete(breaks = c("1", "5", "10"))+
 theme(legend.position="right")#+
 facet_grid(a~type,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))

df.compare_type_all %>% filter (d == 0.9, Ch == 10, Cl == 100, Pv ==1, #a != 0.333333333333333
) %>% 
  ggplot()+ 
  geom_raster(aes(y=a, x = p0, fill = delta_I))+
  scale_fill_gradient2(na.value = "red",low = '#D53E4F', high = '#3288BD',mid = "#FFFFBF",
                       name = "Difference in Income", midpoint = 0
                       ,breaks=c(100,0,-100),
                       #limits=c(1, - 1),
                       labels=c("ABS higher","no difference","RBS higher"))+
  ylab("Level of sensitivity to action (a)") +
  xlab("Probability of biodiversity occuring on patch (P0)")+
  scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
  scale_y_discrete(breaks = c("1", "5", "10"))+
  theme(legend.position="right")#+
 facet_grid(Cl+Ch~a+d,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))
 
 
df.compare_type_all %>% filter (d == 0.9, Ch == 10, Cl == 100, Pv ==1, #a != 0.333333333333333
 ) %>% 
  ggplot()+ 
  geom_raster(aes(y=a, x = p0, fill = delta_B))+
  scale_fill_gradient2(na.value = "red",low = '#D53E4F', high = '#3288BD',mid = "#FFFFBF",
                       name = "Difference in probability\nbiodiversity occurs\nunder scheme participation", midpoint = 0
                       ,breaks=c(100,0,-100),
                       #limits=c(1, - 1),
                       labels=c("ABS higher","no difference","RBS higher"))+
  ylab("Level of sensitivity to action (a)") +
  xlab("Probability of benefit occuring on patch (P0)")+
  scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
  scale_y_discrete(breaks = c("1", "5", "10"))+
  theme(legend.position="right")#+
 facet_grid(Cl+Ch~a+d,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))
 
#df.compare_type_all$a_num <- as.numeric(df.compare_type_all$a)
 #create artifical facetting variable
df.compare_type_all$Comp <- 1
df.compare_type_all$Comp<- as.factor(df.compare_type_all$Comp)

Comp.labs <- c("Difference between ABS and RBS")
names(Comp.labs) <- c("1")
 
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
ggsave("Comp_BiodivGain_aP0L_ABSRBS.png",device='png', dpi=800)


ggarrange(BiodivGain_aP0L_ABS, BiodivGain_aP0L_RBS, Comp_BiodivGain_aP0L_ABSRBS, nrow = 3)


#Expenditures
 #need to reverse the colour code, i.e. show where RBS is Better in red, where ABS i better in blue
 #by a and d, Cl and Ch
df.compare_type_all %>% filter (Ch %in% c(2,10,20),Cl %in% c(25,100,200)
                                 ,d %in% c(0.1, 0.5, 0.9), a %in% c(0.667,1,10)
 ) %>% 
  ggplot()+ 
  geom_raster(aes(y=Pv, x = p0, fill = delta_E))+
  scale_fill_gradient2(na.value = "red",high = '#D53E4F', low = '#3288BD',mid = "#FFFFBF",
                       name = "Difference in Expenditures", midpoint = 0
                       ,breaks=c(0.5,0,- 0.5),
                       #limits=c(1, - 1),
                       labels=c("RBS lower","no difference","ABS lower"))+
  ylab("Level of Pv ") +
  xlab("Probability of benefit occuring on patch (P0)")+
  scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
  theme(legend.position="right")+
 facet_grid(Cl+Ch~a+d,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs,Cl = CL.labs, Ch = Ch.labs))

#by cl and Ch
#check how to depict that better
df.compare_type_all %>% filter (a == 1, d == 0.5,Ch %in% c(2,10,20),Cl %in% c(25,100,200)
) %>% 
 ggplot()+ 
 geom_raster(aes(y=Pv, x = p0, fill = delta_E_scaled))+
 scale_fill_gradient2(na.value = "red",high = '#D53E4F', low = '#3288BD',mid = "#FFFFBF",
                      name = "Difference in Expenditures", midpoint = 0
                      ,breaks=c(0.3,0,- 0.3),
                      #limits=c(1, - 1),
                      labels=c("RBS lower","no difference","ABS lower"))+
 ylab("Level of Pv") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(Ch ~ Cl,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))


#Efficiency
#plot all for with facet a ~d
#y = Pv
#actually we need to scale R for each plot



Eff_a_d_Pv <-df.compare_type_all %>% filter (Ch == 10, Cl == 100,
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
 facet_grid(a ~ d,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))
Eff_a_d_Pv


Eff_a_d_Cl<-df.compare_type_all %>% filter (Ch == 10, Pv == 0.6,
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
 facet_grid(a ~ d,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))
Eff_a_d_Cl


#y = Ch
a.labs.reverse <- c("a = 10", "a = 1","a = 2/3")
names(a.labs.reverse) <- c("10", "1","0.666666666666667")
Eff_a_d_Ch <-df.compare_type_all %>% filter (Cl == 100, Pv == 0.6,
                                d %in% c(0.1, 0.5, 0.9),a %in% c(0.666666666666667,1,10)) %>% 
 ggplot()+ 
 geom_raster(aes(y=Ch, x = p0, fill = delta_R))+
 scale_fill_gradient2(na.value = "red",low = '#D53E4F', high = '#3288BD',mid = "#FFFFBF",
                      name = "Difference in Efficiency", midpoint = 0
                      ,breaks=c(0.0002,0,- 0.0002),
                      #limits=c(1, - 1),
                      labels=c("ABS higher","no difference","RBS higher"))+
 #ylab("Cost of agency monitoring/ h (Ch)") +
# xlab("Probability of biodiversity occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.2", "0.5", "0.8"))+
 scale_y_discrete(breaks = c("4", "8", "12", "16", "20"))+
 theme(legend.position="right")+
 theme_bw()+
 facet_grid(rev(a) ~ d,labeller = labeller(type = type.labs, robot = robot.labs, rev(a) = a.labs.reverse, d = d.labs, Pv = Pv.labs))+
 theme(
  text = element_text(size = 16),          # Overall text size
  axis.title = element_text(size = 18),    # Axis titles
  axis.text = element_text(size = 14),     # Axis labels
  legend.text = element_text(size = 14),   # Legend text
  legend.title = element_text(size = 16),  # Legend title
  strip.text = element_text(size = 16)     # Facet labels
 )

#updated version
# Convert 'a' into a factor and reverse its levels
df.compare_type_all <- df.compare_type_all %>%
 mutate(a = factor(a, levels = c(10, 1, 0.666666666666667)))  # Reverse levels

a.labs.reverse <- c("a = 10", "a = 1", "a = 2/3")
names(a.labs.reverse) <- c("10", "1", "0.666666666666667")


Eff_a_d_Ch <- df.compare_type_all %>%
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
  labels = c("ABS higher", "no difference", "RBS higher")
 ) +
 scale_x_discrete(breaks = c("0.2", "0.5", "0.8")) +
 scale_y_discrete(breaks = c("4", "8", "12", "16", "20")) +
 theme(legend.position = "right") +
 theme_bw() +
 facet_grid(a ~ d, labeller = labeller(a = a.labs.reverse, d = d.labs, Pv = Pv.labs)) +  # No need to reverse 'a' here
 theme(
  text = element_text(size = 16),
  axis.title = element_text(size = 18),
  axis.text = element_text(size = 14),
  legend.text = element_text(size = 14),
  legend.title = element_text(size = 16),
  strip.text = element_text(size = 16)
 )
Eff_a_d_Ch
ggsave("Eff_a_d_Ch_en.png",device='png', dpi=1000, width = 22, height = 15, units = "cm")

#in german for poster
a.labs_de <- c("niedrig", "mittel","hoch")
names(a.labs_de) <- c("0.666666666666667", "1", "10") #0.666666666666667

d.labs_de <- c("niedrig","mittel","hoch")
names(d.labs_de) <- c("0.1","0.5","0.9")

Eff_a_d_Ch_de <-df.compare_type_all %>% filter (Cl == 100, Pv == 0.6,
                                             d %in% c(0.1, 0.5, 0.9),a %in% c(0.666666666666667,1,10)) %>% 
 ggplot()+ 
 geom_raster(aes(y=Ch, x = p0, fill = delta_R))+
 scale_fill_gradient2(na.value = "red",low = '#D53E4F', high = '#3288BD',mid = "#FFFFBF",
                      name = "Unterschied in Effizienz", midpoint = 0
                      ,breaks=c(0.0002,0,- 0.0002),
                      #limits=c(1, - 1),
                      labels=c("PO-AUKMs höher","kein Unterschied","EO-AUKMs höher"))+
 ylab("Monitoringkosten/Stunde") +
 xlab("Wahrscheinlichkeit, dass Biodiversität bereits vor Maßnahme auf Feld")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 theme_bw()+
 facet_grid(a ~ d,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs_de, d = d.labs_de, Pv = Pv.labs))
Eff_a_d_Ch_de

ggsave("Eff_a_d_Ch_de.png",device='png', dpi=800)

#check finally for interaction between Cl, Ch and Pv (i.e. fix a and d)
Eff_Ch_d_Pv <- df.compare_type_all %>% filter (a %in% c(0.666666666666667,1,10), d %in% c(0.1, 0.5, 0.9),
                                Ch %in% c(2,10,20),
                                Cl == 100) %>% 
 ggplot()+ 
 geom_raster(aes(y=interaction(Pv,Ch, sep = "_"), x = p0, fill = delta_R_scaled))+
 scale_fill_gradient2(na.value = "red",low = '#D53E4F', high = '#3288BD',mid = "#FFFFBF",
                      name = "Difference in Efficiency", midpoint = 0
                      ,breaks=c(0.3,0,- 0.3),
                      #limits=c(1, - 1),
                      labels=c("ABS higher","no difference","RBS higher"))+
 ylab("Probability of farm visit_Agency monitoring costs/h") +
 xlab("Probability of biodiversity occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 #scale_y_discrete(breaks = c("0.2_2", "0.6_2", "1_2", "0.2_10", "0.6_10", "1_10", "0.2_20", "0.6_20", "1_20"))+
  theme(legend.position="right")+
 theme_bw()+
 facet_grid(a ~ d,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs, Ch = Ch.labs, Cl = CL.labs))

Eff_Ch_Cl_d <- df.compare_type_all %>% filter (Pv == 0.6, a == 1,
                                                Ch %in% c(2,10,20),
                                                Cl %in% c(50,100,200)) %>% 
 ggplot()+ 
 geom_raster(aes(y=d, x = p0, fill = delta_R_scaled))+
 scale_fill_gradient2(na.value = "red",low = '#D53E4F', high = '#3288BD',mid = "#FFFFBF",
                      name = "Difference in Efficiency", midpoint = 0
                      ,breaks=c(0.3,0,- 0.3),
                      #limits=c(1, - 1),
                      labels=c("ABS higher","no difference","RBS higher"))+
 ylab("Level of d") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(Ch ~ Cl,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs, Ch = Ch.labs, Cl = CL.labs))

Eff_Ch_Cl_a <- df.compare_type_all %>% filter (Pv == 0.6, d == 0.5,
                                               Ch %in% c(2,10,20),
                                               Cl %in% c(50,100,200)) %>% 
 ggplot()+ 
 geom_raster(aes(y=a, x = p0, fill = delta_R_scaled))+
 scale_fill_gradient2(na.value = "red",low = '#D53E4F', high = '#3288BD',mid = "#FFFFBF",
                      name = "Difference in Efficiency", midpoint = 0
                      ,breaks=c(0.3,0,- 0.3),
                      #limits=c(1, - 1),
                      labels=c("ABS higher","no difference","RBS higher"))+
 ylab("Level of a") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(Ch ~ Cl,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs, Ch = Ch.labs, Cl = CL.labs))


Eff_a_d_Ch
Eff_a_d_Cl
Eff_a_d_Pv
Eff_Ch_Cl_Pv #probably appendix
Eff_Ch_Cl_d #probably appendix
Eff_Ch_Cl_a
Eff_Ch_d_Pv

#how to plot results best?
#Pv is closely related to d and Ch
#Cl actually leads to the same effect for both schemes 
#a always has an effect
#Cl, Pv, Ch and d drive agency expenditure
#gain is mainly driven by a 
# a and d describe actual abilties of the robot
#Cl, Ch and Pv rather external factors (Cl) and PES scheme design choices (Ch & Pv)


#how would these 5 plots look in the binary outcome?
#not much informational value!
#####
df.compare_type_all %>% filter (x == 10, Ch == 10, Cl == 100,
                                             d %in% c(0.1, 0.5, 0.9)) %>% 
 ggplot()+ 
 geom_raster(aes(y=Pv, x = p0, fill = delta_R_b))+
 scale_fill_manual(values=myColours)+
 ylab("Level of Pv") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(a ~ d,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))


df.compare_type_all %>% filter (x == 10, Ch == 10, Pv == 0.6,
                                            d %in% c(0.1, 0.5, 0.9)) %>% 
 ggplot()+ 
 geom_raster(aes(y=Cl, x = p0, fill = delta_R_b))+
 scale_fill_manual(values=myColours)+
 ylab("Level of Cl") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(a ~ d,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))



#y = Ch
df.compare_type_all %>% filter (x == 10, Cl == 100, Pv == 0.6,
                                             d %in% c(0.1, 0.5, 0.9)) %>% 
 ggplot()+ 
 geom_raster(aes(y=Ch, x = p0, fill = delta_R_b))+
 scale_fill_manual(values=myColours)+
 ylab("Level of Ch") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(a ~ d,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))

#check finally for interaction between Cl, Ch and Pv (i.e. fix a and d)
df.compare_type_all %>% filter (x == 10, a == 1, d == 0.5,
                                                Ch %in% c(2,10,20),
                                                Cl %in% c(25,100,200)) %>% 
 ggplot()+ 
 geom_raster(aes(y=Pv, x = p0, fill = delta_R_b))+
 scale_fill_manual(values=myColours)+
 ylab("Level of Pv") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(Ch ~ Cl,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs, Ch = Ch.labs, Cl = CL.labs))

df.compare_type_all %>% filter (x == 10, Pv == 0.6, a == 1,
                                               Ch %in% c(2,10,20),
                                               Cl %in% c(25,100,200)) %>% 
 ggplot()+ 
 geom_raster(aes(y=d, x = p0, fill = delta_R_b))+
 scale_fill_manual(values=myColours)+
 ylab("Level of Pv") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(Ch ~ Cl,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs, Ch = Ch.labs, Cl = CL.labs))















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
 ylab("Level of d") +
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





#####old
#######
#as continious output
#rescale delta_E_scaled only for the data used in the plots
data_for_final_plots_d_a_pv <- df.compare_type_all %>% filter (Ch != 0,x!= 0, Cl != 0, 
                                                               Cl == 100, Ch == 10, x == 15, #d %in% c(0.1, 0.5,0.9),
                                                               Pv %in% c(0.2, 0.6,1))

data_for_final_plots_d_a_pv$delta_E_scaled<- ifelse(data_for_final_plots_d_a_pv$delta_E < 0 ,
                                                     -data_for_final_plots_d_a_pv$delta_E/min(data_for_final_plots_d_a_pv$delta_E, na.rm = T),
                                                     data_for_final_plots_d_a_pv$delta_E/max(data_for_final_plots_d_a_pv$delta_E, na.rm = T))
data_for_final_plots_d_a_pv$delta_G_scaled<- ifelse(data_for_final_plots_d_a_pv$delta_G < 0 ,
                                                    -data_for_final_plots_d_a_pv$delta_G/min(data_for_final_plots_d_a_pv$delta_G, na.rm = T),
                                                    data_for_final_plots_d_a_pv$delta_G/max(data_for_final_plots_d_a_pv$delta_G, na.rm = T))
data_for_final_plots_d_a_pv$delta_R_scaled<- ifelse(data_for_final_plots_d_a_pv$delta_R < 0 ,
                                                    -data_for_final_plots_d_a_pv$delta_R/min(data_for_final_plots_d_a_pv$delta_R, na.rm = T),
                                                    data_for_final_plots_d_a_pv$delta_R/max(data_for_final_plots_d_a_pv$delta_R, na.rm = T))




#varying d, a and Pv
final_R_d_a_Pv_cont <- data_for_final_plots_d_a_pv %>% 
 ggplot()+ 
 geom_raster(aes(y=d, x = p0, fill = delta_R_scaled))+
 scale_fill_gradient2(na.value = "red",low = '#0072B2', high = '#F0E442',mid = "white",
                      name = "Difference in Efficiency", midpoint = 0
                      ,breaks=c(1,0,- 1),
                      #limits=c(1, - 1),
                      labels=c("ABS higher","no difference","RBS higher"))+
 ylab("Level of d") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(a~Pv,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))

final_R_d_a_Pv_cont
#####
#expednitures
final_E_d_a_Pv_cont<-data_for_final_plots_d_a_pv %>% 
 ggplot()+ 
 geom_raster(aes(y=d, x = p0, fill = delta_E_scaled))+
 scale_fill_gradient2(na.value = "red",low = '#F0E442', high = '#0072B2',mid = "white",
                      name = "Difference in Agency Expenditures", midpoint = 0,
                      breaks=c(1,0,-1),
                      guide=guide_colourbar(reverse = TRUE), 
                      #limits=c(1, - 1),
                      labels=c("RBS lower","no difference","ABS lower")
                      )+
 ylab("Level of d") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(a~Pv,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))

final_E_d_a_Pv_cont


#biodiversity gain
final_G_d_a_Pv_cont<- data_for_final_plots_d_a_pv %>% 
 ggplot()+ 
 geom_raster(aes(y=d, x = p0, fill = delta_G_scaled))+
 scale_fill_gradient2(na.value = "red",low = '#0072B2', high = '#F0E442',mid = "white",
                      name = "Difference in Biodiversity Gain", midpoint = 0
                      ,breaks=c(1,0,- 1),
                      #limits=c(1, - 1),
                      labels=c("ABS higher","no difference","RBS higher"))+
 ylab("Level of d") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(a~Pv,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))

final_G_d_a_Pv_cont

#expenditures and biodivgain
######


#varying Ch and Cl
data_for_final_plots_d_Ch_Cl <- df.compare_type_all %>% filter (Ch != 0,x!= 0, Cl != 0, 
                                                               Ch %in% c(2,10,20), Cl %in% c(25,100,200),
                                                               a == 1, Pv == 0.6, x == 15, 
                                                               d %in% c(0.1, 0.5,0.9))

data_for_final_plots_d_Ch_Cl$delta_E_scaled<- ifelse(data_for_final_plots_d_Ch_Cl$delta_E < 0 ,
                                                    -data_for_final_plots_d_Ch_Cl$delta_E/min(data_for_final_plots_d_Ch_Cl$delta_E, na.rm = T),
                                                    data_for_final_plots_d_Ch_Cl$delta_E/max(data_for_final_plots_d_Ch_Cl$delta_E, na.rm = T))
data_for_final_plots_d_Ch_Cl$delta_G_scaled<- ifelse(data_for_final_plots_d_Ch_Cl$delta_G < 0 ,
                                                    -data_for_final_plots_d_Ch_Cl$delta_G/min(data_for_final_plots_d_Ch_Cl$delta_G, na.rm = T),
                                                    data_for_final_plots_d_Ch_Cl$delta_G/max(data_for_final_plots_d_Ch_Cl$delta_G, na.rm = T))
data_for_final_plots_d_Ch_Cl$delta_R_scaled<- ifelse(data_for_final_plots_d_Ch_Cl$delta_R < 0 ,
                                                    -data_for_final_plots_d_Ch_Cl$delta_R/min(data_for_final_plots_d_Ch_Cl$delta_R, na.rm = T),
                                                    data_for_final_plots_d_Ch_Cl$delta_R/max(data_for_final_plots_d_Ch_Cl$delta_R, na.rm = T))




#efficiency
final_R_Ch_Cl_d_cont <- data_for_final_plots_d_Ch_Cl %>% 
 ggplot()+ 
 geom_raster(aes(y=interaction(d), x = interaction(p0), fill = delta_R_scaled))+
 scale_fill_gradient2(na.value = "red",low = '#0072B2', high = '#F0E442',mid = "white",
                      name = "Difference in Efficiency", midpoint = 0
                      ,breaks=c(1,0,- 1),
                      #limits=c(1, - 1),
                      labels=c("ABS higher","no difference","RBS higher"))+
 ylab("Level of d") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(Ch ~ Cl,labeller = labeller(Ch = Ch.labs, Cl = CL.labs))

final_R_Ch_Cl_d_cont

#expenditures
final_E_Ch_Cl_d_cont <- data_for_final_plots_d_Ch_Cl %>% 
 ggplot()+ 
 geom_raster(aes(y=interaction(d), x = interaction(p0), fill = delta_E_scaled))+
 scale_fill_gradient2(na.value = "red",low = '#F0E442', high = '#0072B2',mid = "white",
                      name = "Difference in Agency Expenditures", midpoint = 0,
                      breaks=c(1,0,-1),
                      guide=guide_colourbar(reverse = TRUE), 
                      #limits=c(1, - 1),
                      labels=c("RBS lower","no difference","ABS lower")
 )+
 ylab("Level of d") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(Ch ~ Cl,labeller = labeller(Ch = Ch.labs, Cl = CL.labs))

final_E_Ch_Cl_d_cont

#biodiveisty gain
final_G_Ch_Cl_d_cont <- data_for_final_plots_d_Ch_Cl %>% 
 ggplot()+ 
 geom_raster(aes(y=interaction(d), x = interaction(p0), fill = delta_G_scaled))+
 scale_fill_gradient2(na.value = "red",low = '#0072B2', high = '#F0E442',mid = "white",
                      name = "Difference in Biodiveristy Gain", midpoint = 0
                      ,breaks=c(1,0,- 1),
                      #limits=c(1, - 1),
                      labels=c("ABS higher","no difference","RBS higher"))+
 ylab("Level of d") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(Ch ~ Cl,labeller = labeller(Ch = Ch.labs, Cl = CL.labs))

final_G_Ch_Cl_d_cont


#check binary again
#first for changing a and d given p0 and x
final_E_d_a_Pv_bi <- df.compare_type_all %>% filter(Cl == 100, Pv == 0.6, Ch == 10, Pv %in% c(0.2, 0.6, 1), d %in% c(0.1, 0.5, 0.9)) %>% 
 ggplot()+ 
 geom_raster(aes(y=x, x = p0, fill = delta_E_b))+
 scale_fill_manual(values=myColours)+
 ylab("Level of d") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(a~d + Pv,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))

final_E_d_a_Pv_bi


final_G_d_a_Pv_bi <- df.compare_type_all %>% filter(Cl == 100,Pv == 0.6, Ch == 10, Pv %in% c(0.2, 0.6, 1), d %in% c(0.1, 0.5, 0.9)) %>% 
 ggplot()+ 
 geom_raster(aes(y=x, x = p0, fill = delta_G_b))+
 scale_fill_manual(values=myColours)+
 ylab("Level of d") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(a~d + Pv,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))

final_G_d_a_Pv_bi

final_R_d_a_Pv_bi <- df.compare_type_all %>% filter(Cl == 100,Pv == 0.6, Ch == 10, Pv %in% c(0.2, 0.6, 1), d %in% c(0.1, 0.5, 0.9)) %>% 
 ggplot()+ 
 geom_raster(aes(y=x, x = p0, fill = delta_R_b))+
 scale_fill_manual(values=myColours)+
 ylab("Level of d") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(a~d + Pv,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))

final_R_d_a_Pv_bi
#then for changing ch, Cl and d
final_E_Ch_Cl_Pv_bi <- df.compare_type_all %>% filter(a == 1,  d == 0.5, Pv %in% c(0.2, 0.6, 1), Cl %in% c(25,100,200), Ch %in% c(2, 10,20)) %>% 
 ggplot()+ 
 geom_raster(aes(y=x, x = p0, fill = delta_E_b))+
 scale_fill_manual(values=myColours)+
 ylab("Level of d") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(Ch ~ Cl + Pv,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))

final_E_Ch_Cl_Pv_bi

final_G_Ch_Cl_Pv_bi <- df.compare_type_all %>% filter(a == 1,  d == 0.5, Pv %in% c(0.2, 0.6, 1), Cl %in% c(25,100,200), Ch %in% c(2, 10,20)) %>% 
 ggplot()+ 
 geom_raster(aes(y=x, x = p0, fill = delta_G_b))+
 scale_fill_manual(values=myColours)+
 ylab("Level of d") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(Ch ~ Cl + Pv,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))

final_G_Ch_Cl_Pv_bi

final_R_Ch_Cl_Pv_bi <- df.compare_type_all %>% filter(a == 1,  d == 0.5, Pv %in% c(0.2, 0.6, 1), Cl %in% c(25,100,200), Ch %in% c(2, 10,20)) %>% 
 ggplot()+ 
 geom_raster(aes(y=x, x = p0, fill = delta_R_b))+
 scale_fill_manual(values=myColours)+
 ylab("Level of d") +
 xlab("Probability of benefit occuring on patch (P0)")+
 scale_x_discrete(breaks = c("0.1", "0.5", "0.9"))+
 theme(legend.position="right")+
 facet_grid(Ch ~ Cl + Pv,labeller = labeller(type = type.labs, robot = robot.labs, a = a.labs, d = d.labs, Pv = Pv.labs))

final_R_Ch_Cl_Pv_bi 


