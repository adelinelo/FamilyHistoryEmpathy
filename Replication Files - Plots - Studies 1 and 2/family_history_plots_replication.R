#### Immigration History Treatment Plots

# Set Working Directory
setwd("")

# Load packages
library(readstata13)
library(ggplot2)
library(ggthemes)
library(grid)
library(gridExtra)
library(psych)
library(car)
library(Hmisc)
library(dotwhisker)
library(dplyr)
library(tidyr)
library(broom)
library(egg)
library(factoextra)
library(foreign)

# Load data
data_wave1<-read.dta13("wave1_analysis_data.dta")
head(data_wave1)
data_wave2<-read.dta13("wave2_analysis_data.dta")
head(data_wave2)
data_wave3<-read.dta13("wave3_analysis_data.dta")
head(data_wave3)
data_pooled<-read.dta13("pooled_analysis_data.dta")
head(pooled_analysis_data)

# Check data
length(data_wave1$not_restrict_immigrant) # 1000
length(data_wave2$not_restrict_immigrant) # 1299 - 25
length(data_wave3$not_restrict_immigrant) # 3840 - 22 (restrict) and 9 (therm)
length(data_pooled$not_restrict_immigrant)
table(data_pooled$not_restrict_immigrant)
table(data_pooled$immigrant_therm) # 6092 (restrict), 5105 (therm)

# Multiplot function
# This function permits multiple plots to share the same legend. 
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}

scaleFUN <- function(x) sprintf("%.1f", x)

#####################
##Main Effect Plots##
#####################

# Wave 1 Not_Restrict_Immigrant Outcome Plot

wave1_restrict_plot <- ggplot(data_wave1, aes(treatment, not_restrict_immigrant, fill = as.factor(treatment))) + 
  stat_summary(fun.y = mean, geom = "bar",width=.75) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", fun.args = list(mult = 2),
               width = .4, alpha = 1,size=1.5) +
  theme_bw() + scale_fill_manual(values = c("0" = "grey85", "1" = "grey55"),
                                 labels = c("Control", "Treatment"),
                                 name = c("")) + 
  scale_y_continuous(labels=scaleFUN) +
  theme(axis.text.x = element_blank(), legend.text = element_text(size = 18),
        axis.ticks.x = element_blank(), legend.position = "bottom",
        axis.text.y = element_text(size = 18),axis.title.y = element_text(size = 15)) + xlab("") + ylab("") +
  ggtitle("Survey 1, (N=1000)") + theme(text = element_text(size=12)) + coord_cartesian(ylim=c(2.3, 3.6))
wave1_restrict_plot

# Wave 2 Not_Restrict_Immigrant Outcome Plot

wave2_restrict_plot <- ggplot(data_wave2, aes(treatment, not_restrict_immigrant, fill = as.factor(treatment))) + 
  stat_summary(fun.y = mean, geom = "bar",width=.75) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", fun.args = list(mult = 2),
               width = .4, alpha = 1,size=1.5) +
  theme_bw() + scale_fill_manual(values = c("0" = "grey85", "1" = "grey55"),
                                 labels = c("Control", "Treatment"),
                                 name = c("")) + 
  scale_y_continuous(labels=scaleFUN) +
  theme(axis.text.x = element_blank(), legend.text = element_text(size = 18),
        axis.ticks.x = element_blank(), legend.position = "bottom",
        axis.text.y = element_text(size = 18),axis.title.y = element_text(size = 15)) + xlab("") + ylab("") +
  ggtitle("Survey 2, (N=1274)") + theme(text = element_text(size=12)) + coord_cartesian(ylim=c(2.3, 3.6))
wave2_restrict_plot

# Wave 3 Immigrant_Restrict Outcome Plot

wave3_restrict_plot <- ggplot(data_wave3, aes(treatment, not_restrict_immigrant, fill = as.factor(treatment))) + 
  stat_summary(fun.y = mean, geom = "bar",width=.75) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", fun.args = list(mult = 2),
               width = .4, alpha = 1,size=1.5) +
  theme_bw() + scale_fill_manual(values = c("0" = "grey85", "1" = "grey55"),
                                 labels = c("Control", "Treatment"),
                                 name = c("")) + 
  scale_y_continuous(labels=scaleFUN) +
  theme(axis.text.x = element_blank(), legend.text = element_text(size = 18),
        axis.ticks.x = element_blank(), legend.position = "bottom",
        axis.text.y = element_text(size = 18),axis.title.y = element_text(size = 15)) + xlab("") + ylab("") +
  ggtitle("Survey 3, (N=3818)") + theme(text = element_text(size=12)) + coord_cartesian(ylim=c(2.3, 3.6))
wave3_restrict_plot

# Pooled Not_Restrict_Immigrant Outcome Plot

pooled_restrict_plot<-ggplot(data_pooled, aes(treatment, not_restrict_immigrant, fill = as.factor(treatment))) + 
  stat_summary(fun.y = mean, geom = "bar",width=.75) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", fun.args = list(mult = 2),
               width = .4, alpha = 1,size=1.5) +
  theme_bw() + scale_fill_manual(values = c("0" = "grey85", "1" = "grey55"),
                                 labels = c("Control", "Treatment"),
                                 name = c("")) + 
  scale_y_continuous(labels=scaleFUN) +
  theme(axis.text.x = element_blank(), legend.text = element_text(size = 18),
        axis.ticks.x = element_blank(), legend.position = "bottom",
        axis.text.y = element_text(size = 18),axis.title.y = element_text(size = 15)) + xlab("") + ylab("") +
  ggtitle("Pooled, (N=6092)") + theme(text = element_text(size=12)) + coord_cartesian(ylim=c(2.3, 3.6))
pooled_restrict_plot

# Wave 2 Feeling Thermometer Plot

wave2_therm_plot <- ggplot(data_wave2, aes(treatment, immigrant_therm, fill = as.factor(treatment))) + 
  stat_summary(fun.y = mean, geom = "bar",width=.75) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", fun.args = list(mult = 2),
               width = .4, alpha = 1,size=1.5) +
  theme_bw() + scale_fill_manual(values = c("0" = "grey85", "1" = "grey55"),
                                 labels = c("Control", "Treatment"),
                                 name = c("")) + 
  scale_y_continuous(labels=scaleFUN) +
  theme(axis.text.x = element_blank(), legend.text = element_text(size = 18),
        axis.ticks.x = element_blank(), legend.position = "bottom",
        axis.text.y = element_text(size = 18),axis.title.y = element_text(size = 18)) + xlab("") + ylab("") +
  ggtitle("Survey 2, (N=1274)") + theme(text = element_text(size=12)) + coord_cartesian(ylim=c(50, 65))
wave2_therm_plot

# Wave 3 Feeling Thermometer Plot

wave3_therm_plot <- ggplot(data_wave3, aes(treatment, immigrant_therm, fill = as.factor(treatment))) + 
  stat_summary(fun.y = mean, geom = "bar",width=.75) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", fun.args = list(mult = 2),
               width = .4, alpha = 1,size=1.5) +
  theme_bw() + scale_fill_manual(values = c("0" = "grey85", "1" = "grey55"),
                                 labels = c("Control", "Treatment"),
                                 name = c("")) + 
  scale_y_continuous(labels=scaleFUN) +
  theme(axis.text.x = element_blank(), legend.text = element_text(size = 18),
        axis.ticks.x = element_blank(), legend.position = "bottom",
        axis.text.y = element_text(size = 18),axis.title.y = element_text(size = 18)) + xlab("") + ylab("") +
  ggtitle("Survey 3, (N=3831)") + theme(text = element_text(size=12)) + coord_cartesian(ylim=c(50, 65))
wave3_therm_plot

# Pooled Feeling Thermometer Plot

pooled_therm_plot <- ggplot(data_pooled, aes(treatment, immigrant_therm, fill = as.factor(treatment))) + 
  stat_summary(fun.y = mean, geom = "bar",width=.75) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", fun.args = list(mult = 2),
               width = .4, alpha = 1,size=1.5) +
  theme_bw() + scale_fill_manual(values = c("0" = "grey85", "1" = "grey55"),
                                 labels = c("Control", "Treatment"),
                                 name = c("")) + 
  scale_y_continuous(labels=scaleFUN) +
  theme(axis.text.x = element_blank(), legend.text = element_text(size = 18),
        axis.ticks.x = element_blank(), legend.position = "bottom",
        axis.text.y = element_text(size = 18),axis.title.y = element_text(size = 18)) + xlab("") + ylab("") +
  ggtitle("Pooled, (N=5105)") + theme(text = element_text(size=12)) + coord_cartesian(ylim=c(50, 65))
pooled_therm_plot

# Combined Plots

grid_arrange_shared_legend(pooled_restrict_plot,wave1_restrict_plot,wave2_restrict_plot,wave3_restrict_plot, ncol =4, nrow = 1)

grid_arrange_shared_legend(pooled_therm_plot,wave2_therm_plot,wave3_therm_plot, ncol =3, nrow = 1)


##################
##Subgroup Plots##
##################

######## Partisanship

## No Restrictions Outcome

restrict_data_partisan<-read.csv("partisanship_plot_restrict.csv")
head(restrict_data_partisan)

# Reordering factor variables
restrict_data_partisan$model<-factor(restrict_data_partisan$model,levels=c("Republican","Independent","Democrat"))
restrict_data_partisan$term<-factor(restrict_data_partisan$term,levels=c("Survey 1","Survey 2","Survey 3"))
# Restrict Plots
restrict_partisan_plot<-small_multiple(restrict_data_partisan, dot_args = list(size=.75,color="black"),whisker_args = list(size = .1,color="black")) +
  theme_bw() +  geom_hline(yintercept = 0, colour = "grey60", linetype = 2,size=.75) +
  ggtitle("Partisan Subgroup Effects: Open Immigration") + 
  theme(plot.title = element_text(size=12),legend.position="none",
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=10),
        strip.text.y=element_text(size=11),
        strip.background = element_rect(fill="grey90"))
restrict_partisan_plot

## Thermometer Outcome

therm_data_partisan<-read.csv("partisan_plot_thermometer.csv")
head(therm_data_partisan)

# Reordering factor variables
therm_data_partisan$model<-factor(therm_data_partisan$model,levels=c("Republican","Independent","Democrat"))
therm_data_partisan$term<-factor(therm_data_partisan$term,levels=c("Survey 2","Survey 3"))
# Thermometer Plots
therm_partisan_plot<-small_multiple(therm_data_partisan, dot_args = list(size=.75,color="black"),whisker_args = list(size = .1,color="black")) +
  theme_bw() +  geom_hline(yintercept = 0, colour = "grey60", linetype = 2,size=.75) +
  ggtitle("Partisan Subgroup Effects: Thermometer") + 
  theme(plot.title = element_text(size=12),legend.position="none",
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=10),
        strip.text.y=element_text(size=11),
        strip.background = element_rect(fill="grey90"))
therm_partisan_plot

ggarrange(restrict_partisan_plot,therm_partisan_plot,ncol=2,nrow=1)

########### Trump Approval

## No Restrictions Outcome

restrict_data_trump<-read.csv("trump_plot_restrict.csv")
head(restrict_data_trump)

# Reordering factor variables
restrict_data_trump$model<-factor(restrict_data_trump$model,levels=c("Approves","Disapproves"))
restrict_data_trump$term<-factor(restrict_data_trump$term,levels=c("Survey 2","Survey 3"))
# Restrict Plots
restrict_trump_plot<-small_multiple(restrict_data_trump, dot_args = list(size=.75,color="black"),whisker_args = list(size = .1,color="black")) +
  theme_bw() +  geom_hline(yintercept = 0, colour = "grey60", linetype = 2,size=.75) +
  ggtitle("Trump Subgroup Effects: Open Immigration") + 
  theme(plot.title = element_text(size=12),legend.position="none",
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=10),
        strip.text.y=element_text(size=11),
        strip.background = element_rect(fill="grey90"))
restrict_trump_plot

## Thermometer Outcome

therm_data_trump<-read.csv("trump_plot_thermometer.csv")
head(therm_data_trump)

# Reordering factor variables
therm_data_trump$model<-factor(therm_data_trump$model,levels=c("Approves","Disapproves"))
therm_data_trump$term<-factor(therm_data_trump$term,levels=c("Survey 2","Survey 3"))
# Thermometer Plots
therm_trump_plot<-small_multiple(therm_data_trump, dot_args = list(size=.75,color="black"),whisker_args = list(size = .1,color="black")) +
  theme_bw() +  geom_hline(yintercept = 0, colour = "grey60", linetype = 2,size=.75) +
  ggtitle("Trump Subgroup Effects: Thermometer") + 
  theme(plot.title = element_text(size=12),legend.position="none",
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=10),
        strip.text.y=element_text(size=11),
        strip.background = element_rect(fill="grey90"))
therm_trump_plot

ggarrange(restrict_partisan_plot,therm_partisan_plot,restrict_trump_plot,therm_trump_plot,ncol=2,nrow=2)


######## Immigration Generation

## No Restrictions Outcome

restrict_data_gen<-read.csv("generation_plot_restrict.csv")
head(restrict_data_gen)

# Reordering factor variables
restrict_data_gen$model<-factor(restrict_data_gen$model,levels=c("Great-Grandparents or earlier","Grandparents or later"))
# Restrict Plots
restrict_gen_plot<-small_multiple(restrict_data_gen, dot_args = list(size=.75,color="black"),whisker_args = list(size = .1,color="black")) +
  theme_bw() +  geom_hline(yintercept = 0, colour = "grey60", linetype = 2,size=.75) +
  ggtitle("Generation Subgroup Effects: Open Immigration") + 
  theme(plot.title = element_text(size=12),legend.position="none",
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=10),
        strip.text.y=element_text(size=11),
        strip.background = element_rect(fill="grey90"))
restrict_gen_plot

## Thermometer Outcome

therm_data_gen<-read.csv("generation_plot_thermometer.csv")
head(therm_data_gen)

# Reordering factor variables
therm_data_gen$model<-factor(therm_data_gen$model,levels=c("Great-Grandparents or earlier","Grandparents or later"))
# Thermometer Plots
therm_gen_plot<-small_multiple(therm_data_gen, dot_args = list(size=.75,color="black"),whisker_args = list(size = .1,color="black")) +
  theme_bw() +  geom_hline(yintercept = 0, colour = "grey60", linetype = 2,size=.75) +
  ggtitle("Generation Subgroup Effects: Thermometer") + 
  theme(plot.title = element_text(size=12),legend.position="none",
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=10),
        strip.text.y=element_text(size=11),
        strip.background = element_rect(fill="grey90"))
therm_gen_plot

ggarrange(restrict_gen_plot,therm_gen_plot,ncol=2,nrow=1)


######## Partisanship Subgroup Plots by Strong, Moderate, and Lean

restrict_data_partisan<-read.csv("strong_partisanship_plot_restrict.csv")
head(restrict_data_partisan)

## No Restrict Outcome

# Reordering factor variables
restrict_data_partisan$model<-factor(restrict_data_partisan$model,levels=c("Strong Republican", "Republican", "Lean Republican","Independent","Lean Democrat","Democrat","Strong Democrat"))
restrict_data_partisan$term<-factor(restrict_data_partisan$term,levels=c("Survey 1","Survey 2","Survey 3"))
# Restrict Plots
restrict_partisan_plot<-small_multiple(restrict_data_partisan, dot_args = list(size=.75,color="black"),whisker_args = list(size = .1,color="black")) +
  theme_bw() +  geom_hline(yintercept = 0, colour = "grey60", linetype = 2,size=.75) +
  ggtitle("Partisan Subgroup Effects: Open Immigration") + 
  theme(plot.title = element_text(size=12),legend.position="none",
        axis.text.x=element_text(size=12,angle = 90, hjust = 1),
        axis.text.y=element_text(size=10),
        strip.text.y=element_text(size=11),
        strip.background = element_rect(fill="grey90"))
restrict_partisan_plot

## Thermometer Outcome

therm_data_partisan<-read.csv("strong_partisanship_plot_thermometer.csv")
head(therm_data_partisan)

# Reordering factor variables
therm_data_partisan$model<-factor(therm_data_partisan$model,levels=c("Strong Republican", "Republican", "Lean Republican","Independent","Lean Democrat","Democrat","Strong Democrat"))
therm_data_partisan$term<-factor(therm_data_partisan$term,levels=c("Survey 2","Survey 3"))
# Thermometer Plots
therm_partisan_plot<-small_multiple(therm_data_partisan, dot_args = list(size=.75,color="black"),whisker_args = list(size = .1,color="black")) +
  theme_bw() +  geom_hline(yintercept = 0, colour = "grey60", linetype = 2,size=.75) +
  ggtitle("Partisan Subgroup Effects: Thermometer") + 
  theme(plot.title = element_text(size=12),legend.position="none",
        axis.text.x=element_text(size=12,angle = 90, hjust = 1),
        axis.text.y=element_text(size=10),
        strip.text.y=element_text(size=11),
        strip.background = element_rect(fill="grey90"))
therm_partisan_plot

ggarrange(restrict_partisan_plot,therm_partisan_plot,ncol=2,nrow=1)
