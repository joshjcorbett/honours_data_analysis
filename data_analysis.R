###############
#DATA ANALYSIS#
###############

##E1##

#Set contrasts appropriately for all experiments.

options(contrasts = c("contr.sum","contr.poly"))

#Ensure working directory is set to local data path where data files are stored.

dhf <- read_csv("exp1_data.csv")

#Create plot of absolute error by participant, background condition, and number of targets.

plot_error <- ggplot( dhf,
                aes(x = as.factor(no_targets), y = euclidean_dist, color = noise_present)) +
                stat_summary(fun = mean, geom = "point", size=3, alpha=0.82, position = position_dodge2(width = .5)) +
                stat_summary(fun.data = mean_cl_boot, fun.args=(conf.int=0.95), geom="errorbar", size=.9, alpha=0.82, position = "dodge2", width=0.5) +
                labs(color = "Background") + xlab("No. Targets") + ylab("Absolute Error (deg)") +
                facet_grid(participant~.)

plot_error <- plot_error + scale_colour_manual(values=wes_palette(n=4, name="Royal1")) + theme(axis.text.y   = element_text(size=14),
                                                                                               axis.text.x   = element_text(size=14),
                                                                                               axis.title.y  = element_text(size=16, margin = margin(r = 15)),
                                                                                               axis.title.x  = element_text(size=16),
                                                                                               legend.title = element_text(size=16),
                                                                                               legend.text = element_text(size=14),
                                                                                               panel.background = element_blank(),
                                                                                               panel.grid.major = element_blank(),
                                                                                               panel.grid.minor = element_blank(),
                                                                                               axis.line = element_line(colour = "darkgray"),
                                                                                               panel.border = element_rect(colour = "darkgray", fill=NA, size=1)
)



show(plot_error)

#Create plot of forward/backward displacement by participant, background condition, and number of targets.

plot <- ggplot( dhf,
                aes(x = as.factor(no_targets), y = amountExtrapolation, color = noise_present)) +
                stat_summary(fun = mean, geom = "point", size=3, alpha=0.82, position = position_dodge2(width = .5)) +
                stat_summary(fun.data = mean_cl_boot, fun.args=(conf.int=0.95), geom="errorbar", size=.9, alpha=0.82, position = "dodge2", width=0.5) +
                labs(color = "Background") + geom_hline(yintercept=0, linetype=2) + xlab("No. Targets") + ylab("Distance from the Finish Line (deg)") +
                facet_grid(participant~.)

plot <- plot + scale_colour_manual(values=wes_palette(n=4, name="Royal1")) + theme(axis.text.y   = element_text(size=14),
                                                                                   axis.text.x   = element_text(size=14),
                                                                                   axis.title.y  = element_text(size=16, margin = margin(r = 15)),
                                                                                   axis.title.x  = element_text(size=16),
                                                                                   legend.title = element_text(size=16),
                                                                                   legend.text = element_text(size=14),
                                                                                   panel.background = element_blank(),
                                                                                   panel.grid.major = element_blank(),
                                                                                   panel.grid.minor = element_blank(),
                                                                                   axis.line = element_line(colour = "darkgray"),
                                                                                   panel.border = element_rect(colour = "darkgray", fill=NA, size=1)
)

show(plot)

#Create plot of eccentricity vs. forward/backward displacement by background condition.

gg <- ggplot(dhf, aes(x=eccentricity_deg, y=amountExtrapolation, color=noise_present)) +
  geom_point(shape = 1) + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + facet_grid(participant~.) +
  xlab("Eccentricity (deg)") + ylab("Distance from the Finish Line (deg)")
gg <- gg + labs(color = "Background")

gg <- gg + scale_colour_manual(values=c("dodgerblue", "coral")) + theme(axis.text.y   = element_text(size=16),
                                                                        axis.text.x   = element_text(size=16),
                                                                        axis.title.y  = element_text(size=20),
                                                                        axis.title.x  = element_text(size=20),
                                                                        legend.title = element_text(size=20),
                                                                        legend.text = element_text(size = 16),
                                                                        panel.background = element_blank(),
                                                                        panel.grid.major = element_blank(),
                                                                        panel.grid.minor = element_blank(),
                                                                        axis.line = element_line(colour = "darkgray"),
                                                                        panel.border = element_rect(colour = "darkgray", fill=NA, size=1)
)

gg <- gg + scale_y_continuous(breaks = c(-4, -2, 0, 2, 4))

show(gg)

#Create plot of eccentricity vs. displacement by speed to fovea.

gg2 <- ggplot(dhf, aes(x=eccentricity_deg, y=amountExtrapolation, color=velocityToFovea)) +
  geom_point(shape=1) + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + facet_grid(participant~.) +
  xlab("Eccentricity (deg)") + ylab("Distance from the Finish Line (deg)")

gg2 <- gg2 + labs(color = "Speed to Fovea (deg/s)")

gg2 <- gg2 + scale_fill_gradient(low =  "#00AFBB", high = "#E7B800", aesthetics = "col") + theme(axis.text.y   = element_text(size=16),
                                                                                                 axis.text.x   = element_text(size=16),
                                                                                                 axis.title.y  = element_text(size=20),
                                                                                                 axis.title.x  = element_text(size=20),
                                                                                                 legend.title = element_text(size=20),
                                                                                                 legend.text = element_text(size=16),
                                                                                                 panel.background = element_blank(),
                                                                                                 panel.grid.major = element_blank(),
                                                                                                 panel.grid.minor = element_blank(),
                                                                                                 axis.line = element_line(colour = "darkgray"),
                                                                                                 panel.border = element_rect(colour = "darkgray", fill=NA, size=1)
)

gg2 <- gg2 + scale_y_continuous(breaks = c(-4, -2, 0, 2, 4))

show(gg2)

#Data Analysis

#Initial ANOVA (no. targets x background condition)

finish_line <- lm(amountExtrapolation ~ no_targets*noise_present, data = dhf)

drop1(finish_line, .~., test="F")

summary(finish_line)

#First regression (includes eccentricity and relevant interactions)

model_eccentricity <- lm(dhf, formula = amountExtrapolation ~ noise_present + no_targets + noise_present*no_targets + eccentricity + eccentricity*noise_present)
summary(model_eccentricity)

#Second regression (also includes speed to fovea)

model2 <- lm(dhf, formula = amountExtrapolation ~ noise_present + no_targets + noise_present*no_targets + eccentricity + velocityToFovea + eccentricity*velocityToFovea)
summary(model2)

## E2a ##

dhf <- read_csv("exp2a_data.csv")

#Create plot of absolute error by participant, background condition, and number of targets.

plot_error <- ggplot( dhf,
                      aes(x = as.factor(no_targets), y = euclidean_dist, color = noise_present)) +
  stat_summary(fun = mean, geom = "point", size=3, alpha=0.82, position = position_dodge2(width = .5)) +
  stat_summary(fun.data = mean_cl_boot, fun.args=(conf.int=0.95), geom="errorbar", size=.9, alpha=0.82, position = "dodge2", width=0.5) +
  labs(color = "Background") + xlab("No. Targets") + ylab("Absolute Error (deg)") +
  facet_grid(participant~.)

plot_error <- plot_error + scale_colour_manual(values=wes_palette(n=4, name="Royal1")) + theme(axis.text.y   = element_text(size=14),
                                                                                               axis.text.x   = element_text(size=14),
                                                                                               axis.title.y  = element_text(size=16, margin = margin(r = 15)),
                                                                                               axis.title.x  = element_text(size=16),
                                                                                               legend.title = element_text(size=16),
                                                                                               legend.text = element_text(size=14),
                                                                                               panel.background = element_blank(),
                                                                                               panel.grid.major = element_blank(),
                                                                                               panel.grid.minor = element_blank(),
                                                                                               axis.line = element_line(colour = "darkgray"),
                                                                                               panel.border = element_rect(colour = "darkgray", fill=NA, size=1)
)



show(plot_error)

#Create plot of forward/backward displacement by participant, background condition, and number of targets.

plot <- ggplot( dhf,
                aes(x = as.factor(no_targets), y = amountExtrapolation, color = noise_present)) +
  stat_summary(fun = mean, geom = "point", size=3, alpha=0.82, position = position_dodge2(width = .5)) +
  stat_summary(fun.data = mean_cl_boot, fun.args=(conf.int=0.95), geom="errorbar", size=.9, alpha=0.82, position = "dodge2", width=0.5) +
  labs(color = "Background") + geom_hline(yintercept=0, linetype=2) + xlab("No. Targets") + ylab("Distance from the Finish Line (deg)") +
  facet_grid(participant~.)

plot <- plot + scale_colour_manual(values=wes_palette(n=4, name="Royal1")) + theme(axis.text.y   = element_text(size=14),
                                                                                   axis.text.x   = element_text(size=14),
                                                                                   axis.title.y  = element_text(size=16, margin = margin(r = 15)),
                                                                                   axis.title.x  = element_text(size=16),
                                                                                   legend.title = element_text(size=16),
                                                                                   legend.text = element_text(size=14),
                                                                                   panel.background = element_blank(),
                                                                                   panel.grid.major = element_blank(),
                                                                                   panel.grid.minor = element_blank(),
                                                                                   axis.line = element_line(colour = "darkgray"),
                                                                                   panel.border = element_rect(colour = "darkgray", fill=NA, size=1)
)

show(plot)

#Create plot of eccentricity vs. forward/backward displacement by background condition.

gg <- ggplot(dhf, aes(x=eccentricity_deg, y=amountExtrapolation, color=noise_present)) +
  geom_point(shape = 1) + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + facet_grid(participant~.) +
  xlab("Eccentricity (deg)") + ylab("Distance from the Finish Line (deg)")
gg <- gg + labs(color = "Background")

gg <- gg + scale_colour_manual(values=c("dodgerblue", "coral")) + theme(axis.text.y   = element_text(size=16),
                                                                        axis.text.x   = element_text(size=16),
                                                                        axis.title.y  = element_text(size=20),
                                                                        axis.title.x  = element_text(size=20),
                                                                        legend.title = element_text(size=20),
                                                                        legend.text = element_text(size = 16),
                                                                        panel.background = element_blank(),
                                                                        panel.grid.major = element_blank(),
                                                                        panel.grid.minor = element_blank(),
                                                                        axis.line = element_line(colour = "darkgray"),
                                                                        panel.border = element_rect(colour = "darkgray", fill=NA, size=1)
)

gg <- gg + scale_y_continuous(breaks = c(-4, -2, 0, 2, 4))

show(gg)

#Create plot of eccentricity vs. displacement by speed to fovea.

gg2 <- ggplot(dhf, aes(x=eccentricity_deg, y=amountExtrapolation, color=velocityToFovea)) +
  geom_point(shape=1) + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + facet_grid(participant~.) +
  xlab("Eccentricity (deg)") + ylab("Distance from the Finish Line (deg)")

gg2 <- gg2 + labs(color = "Speed to Fovea (deg/s)")

gg2 <- gg2 + scale_fill_gradient(low =  "#00AFBB", high = "#E7B800", aesthetics = "col") + theme(axis.text.y   = element_text(size=16),
                                                                                                 axis.text.x   = element_text(size=16),
                                                                                                 axis.title.y  = element_text(size=20),
                                                                                                 axis.title.x  = element_text(size=20),
                                                                                                 legend.title = element_text(size=20),
                                                                                                 legend.text = element_text(size=16),
                                                                                                 panel.background = element_blank(),
                                                                                                 panel.grid.major = element_blank(),
                                                                                                 panel.grid.minor = element_blank(),
                                                                                                 axis.line = element_line(colour = "darkgray"),
                                                                                                 panel.border = element_rect(colour = "darkgray", fill=NA, size=1)
)

gg2 <- gg2 + scale_y_continuous(breaks = c(-4, -2, 0, 2, 4))

show(gg2)

#Data Analysis

#Initial ANOVA

finish_line <- lm(amountExtrapolation ~ noise_present*no_targets*line_orientation, data = dhf)

drop1(finish_line, .~., test="F")

#Regression including eccentricity/velocity to fovea

model2 <- lm(dhf, formula = amountExtrapolation ~ noise_present + no_targets + noise_present*no_targets + eccentricity + velocityToFovea + eccentricity*velocityToFovea)
summary(model2)

##E2b##

dhf <- read_csv("exp2b_data.csv")

#Create plot of absolute error by participant, background condition, and number of targets.

plot_error <- ggplot( dhf,
                      aes(x = as.factor(no_targets), y = euclidean_dist, color = noise_present)) +
  stat_summary(fun = mean, geom = "point", size=3, alpha=0.82, position = position_dodge2(width = .5)) +
  stat_summary(fun.data = mean_cl_boot, fun.args=(conf.int=0.95), geom="errorbar", size=.9, alpha=0.82, position = "dodge2", width=0.5) +
  labs(color = "Background") + xlab("No. Targets") + ylab("Absolute Error (deg)") +
  facet_grid(participant~.)

plot_error <- plot_error + scale_colour_manual(values=wes_palette(n=4, name="Royal1")) + theme(axis.text.y   = element_text(size=14),
                                                                                               axis.text.x   = element_text(size=14),
                                                                                               axis.title.y  = element_text(size=16, margin = margin(r = 15)),
                                                                                               axis.title.x  = element_text(size=16),
                                                                                               legend.title = element_text(size=16),
                                                                                               legend.text = element_text(size=14),
                                                                                               panel.background = element_blank(),
                                                                                               panel.grid.major = element_blank(),
                                                                                               panel.grid.minor = element_blank(),
                                                                                               axis.line = element_line(colour = "darkgray"),
                                                                                               panel.border = element_rect(colour = "darkgray", fill=NA, size=1)
)



show(plot_error)

#Create plot of forward/backward displacement by participant, background condition, and number of targets.

plot <- ggplot( dhf,
                aes(x = as.factor(no_targets), y = amountExtrapolation, color = noise_present)) +
  stat_summary(fun = mean, geom = "point", size=3, alpha=0.82, position = position_dodge2(width = .5)) +
  stat_summary(fun.data = mean_cl_boot, fun.args=(conf.int=0.95), geom="errorbar", size=.9, alpha=0.82, position = "dodge2", width=0.5) +
  labs(color = "Background") + geom_hline(yintercept=0, linetype=2) + xlab("No. Targets") + ylab("Distance from the Finish Line (deg)") +
  facet_grid(participant~.)

plot <- plot + scale_colour_manual(values=wes_palette(n=4, name="Royal1")) + theme(axis.text.y   = element_text(size=14),
                                                                                   axis.text.x   = element_text(size=14),
                                                                                   axis.title.y  = element_text(size=16, margin = margin(r = 15)),
                                                                                   axis.title.x  = element_text(size=16),
                                                                                   legend.title = element_text(size=16),
                                                                                   legend.text = element_text(size=14),
                                                                                   panel.background = element_blank(),
                                                                                   panel.grid.major = element_blank(),
                                                                                   panel.grid.minor = element_blank(),
                                                                                   axis.line = element_line(colour = "darkgray"),
                                                                                   panel.border = element_rect(colour = "darkgray", fill=NA, size=1)
)

show(plot)

#Create plot of eccentricity vs. forward/backward displacement by background condition.

gg <- ggplot(dhf, aes(x=eccentricity_deg, y=amountExtrapolation, color=noise_present)) +
  geom_point(shape = 1) + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + facet_grid(participant~.) +
  xlab("Eccentricity (deg)") + ylab("Distance from the Finish Line (deg)")
gg <- gg + labs(color = "Background")

gg <- gg + scale_colour_manual(values=c("dodgerblue", "coral")) + theme(axis.text.y   = element_text(size=16),
                                                                        axis.text.x   = element_text(size=16),
                                                                        axis.title.y  = element_text(size=20),
                                                                        axis.title.x  = element_text(size=20),
                                                                        legend.title = element_text(size=20),
                                                                        legend.text = element_text(size = 16),
                                                                        panel.background = element_blank(),
                                                                        panel.grid.major = element_blank(),
                                                                        panel.grid.minor = element_blank(),
                                                                        axis.line = element_line(colour = "darkgray"),
                                                                        panel.border = element_rect(colour = "darkgray", fill=NA, size=1)
)

gg <- gg + scale_y_continuous(breaks = c(-4, -2, 0, 2, 4))

show(gg)

#Create plot of eccentricity vs. displacement by speed to fovea.

gg2 <- ggplot(dhf, aes(x=eccentricity_deg, y=amountExtrapolation, color=velocityToFovea)) +
  geom_point(shape=1) + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + facet_grid(participant~.) +
  xlab("Eccentricity (deg)") + ylab("Distance from the Finish Line (deg)")

gg2 <- gg2 + labs(color = "Speed to Fovea (deg/s)")

gg2 <- gg2 + scale_fill_gradient(low =  "#00AFBB", high = "#E7B800", aesthetics = "col") + theme(axis.text.y   = element_text(size=16),
                                                                                                 axis.text.x   = element_text(size=16),
                                                                                                 axis.title.y  = element_text(size=20),
                                                                                                 axis.title.x  = element_text(size=20),
                                                                                                 legend.title = element_text(size=20),
                                                                                                 legend.text = element_text(size=16),
                                                                                                 panel.background = element_blank(),
                                                                                                 panel.grid.major = element_blank(),
                                                                                                 panel.grid.minor = element_blank(),
                                                                                                 axis.line = element_line(colour = "darkgray"),
                                                                                                 panel.border = element_rect(colour = "darkgray", fill=NA, size=1)
)

gg2 <- gg2 + scale_y_continuous(breaks = c(-4, -2, 0, 2, 4))

show(gg2)

#Data Analysis

#Initial ANOVA

finish_line <- lm(amountExtrapolation ~ noise_present*no_targets*line_orientation, data = dhf)

drop1(finish_line, .~., test="F")

#Regression including eccentricity/velocity to fovea

model2 <- lm(dhf, formula = amountExtrapolation ~ noise_present + no_targets + noise_present*no_targets + eccentricity + velocityToFovea + eccentricity*velocityToFovea)
summary(model2)
          