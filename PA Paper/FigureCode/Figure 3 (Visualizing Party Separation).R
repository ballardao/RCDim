rm(list = ls())
setwd("~/Dropbox/Dimensionality/PARevision/EmpiricalCode/DataFiles")

library(foreign)
library(ggplot2)
library(grid)
theme_set(theme_bw(base_size = 9))

MyPalette <- colorRampPalette(c("#000000", "#FFFFFF"))

# Load historical NOMINATE estimates
NOM <- read.dta("Historic NOMINATE Estimates.DTA")
NOM$chamber <- "House"
NOM$chamber[NOM$cd == 0] <- "Senate"
NOM$FirstYear <- (NOM$cong - 25) * 2 + 1837
NOM$LastYear <- (NOM$cong - 25) * 2 + 1837 + 1
NOM2P <- NOM[is.element(NOM$party, c(100, 200)) & NOM$cong > 60, ]

# Generate density curves for different levels of simulated party separation
NSims <- 100
XPositions <- seq(-4, 4, by = 0.01)
NSims <- length(XPositions)
BasicUnit <- data.frame(x = XPositions, y = dnorm(XPositions))

Generator2 <- function(x){
  party <- rep(c(1, -1), each = NSims)
  position <- rbind(BasicUnit, BasicUnit)
  position$x <- round(position$x + party * x / 2, 3)

  party[party == -1] <- "Dem"
  party[party == 1] <- "Rep"
  Generated <- data.frame(Separation = paste("Sep. =", x), position, party)
  colnames(Generated) <- c("Separation", "x", "y", "Party")
  return(Generated)
  }

Parameters <- c(0, 1/2, 1, 2, 3, 4, 6, 8, 10, 12)  # Party separation levels to sweep
Simulated <- do.call(rbind, lapply(Parameters, Generator2))  # Stack curves into data.frame

# Generate density curves for empirical party separation, 86th Senate
From <- min(NOM2P$dwnom1[NOM2P$cong == 86 & NOM2P$chamber == "Senate"], na.rm = T)
To <- max(NOM2P$dwnom1[NOM2P$cong == 86 & NOM2P$chamber == "Senate"], na.rm = T)
Empirical2AD <- data.frame(x = density(NOM2P$dwnom1[NOM2P$cong == 86 & NOM2P$chamber == "Senate" & NOM2P$party == 100], from = From, to = To, n = NSims)$x)
Empirical2AD$y <- density(NOM2P$dwnom1[NOM2P$cong == 86 & NOM2P$chamber == "Senate" & NOM2P$party == 100], from = From, to = To, n = NSims)$y
Empirical2AD <- rbind(Empirical2AD[1, ], Empirical2AD, Empirical2AD[nrow(Empirical2AD), ])
Empirical2AD[c(1, nrow(Empirical2AD)), "y"] <- 0
Empirical2AR <- data.frame(x = density(NOM2P$dwnom1[NOM2P$cong == 86 & NOM2P$chamber == "Senate" & NOM2P$party == 200], from = From, to = To, n = NSims)$x)
Empirical2AR$y <- density(NOM2P$dwnom1[NOM2P$cong == 86 & NOM2P$chamber == "Senate" & NOM2P$party == 200], from = From, to = To, n = NSims)$y
Empirical2AR <- rbind(Empirical2AR[1, ], Empirical2AR, Empirical2AR[nrow(Empirical2AR), ])
Empirical2AR[c(1, nrow(Empirical2AR)), "y"] <- 0
Empirical2AD$Party <- "Dem"
Empirical2AR$Party <- "Rep"
Empirical2AD$Separation <- "86th Senate"
Empirical2AR$Separation <- "86th Senate"
Empirical2A <- data.frame(rbind(Empirical2AD, Empirical2AR)[, colnames(Simulated)])

# Generate density curves for empirical party separation, 109th House
From <- min(NOM2P$dwnom1[NOM2P$cong == 109 & NOM2P$chamber == "House"], na.rm = T)
To <- max(NOM2P$dwnom1[NOM2P$cong == 109 & NOM2P$chamber == "House"], na.rm = T)
Empirical2BD <- data.frame(x = density(NOM2P$dwnom1[NOM2P$cong == 109 & NOM2P$chamber == "House" & NOM2P$party == 100], from = From, to = To, n = NSims)$x)
Empirical2BD$y <- density(NOM2P$dwnom1[NOM2P$cong == 109 & NOM2P$chamber == "House" & NOM2P$party == 100], from = From, to = To, n = NSims)$y
Empirical2BD <- rbind(Empirical2BD[1, ], Empirical2BD, Empirical2BD[nrow(Empirical2BD), ])
Empirical2BD[c(1, nrow(Empirical2BD)), "y"] <- 0
Empirical2BR <- data.frame(x = density(NOM2P$dwnom1[NOM2P$cong == 86 & NOM2P$chamber == "Senate" & NOM2P$party == 200], from = From, to = To, n = NSims)$x)
Empirical2BR$y <- density(NOM2P$dwnom1[NOM2P$cong == 86 & NOM2P$chamber == "Senate" & NOM2P$party == 200], from = From, to = To, n = NSims)$y
Empirical2BR <- rbind(Empirical2BR[1, ], Empirical2BR, Empirical2BR[nrow(Empirical2BR), ])
Empirical2BR[c(1, nrow(Empirical2BR)), "y"] <- 0
Empirical2BD$Party <- "Dem"
Empirical2BR$Party <- "Rep"
Empirical2BD$Separation <- "109th House"
Empirical2BR$Separation <- "109th House"
Empirical2B <- data.frame(rbind(Empirical2BD, Empirical2BR)[, colnames(Simulated)])

# Stack simulated and empirical curves into the same data.frame
Combined2 <- data.frame(rbind(Simulated, Empirical2A, Empirical2B))

# Find density curves that combine the two parties' densities together
# (plotted in dark gray)
Joint <- Combined2[!duplicated(Combined2[, c("Separation", "x")]), ]
Joint$y <- by(Combined2$y, paste(Combined2$Separation, Combined2$x), sum)[paste(Joint$Separation, Joint$x)]
Joint$Party <- "Combined"
Maxes <- Joint[by(Joint$x, Joint$Separation, max)[Joint$Separation] == Joint$x, ]
Maxes$y <- 0
Maxes <- Maxes[regexpr("th", Maxes$Separation) != -1, ]
Mins <- Joint[by(Joint$x, Joint$Separation, min)[Joint$Separation] == Joint$x, ]
Mins$y <- 0
Mins <- Mins[regexpr("th", Mins$Separation) != -1, ]
Joint <- data.frame(rbind(Joint, Maxes, Mins))
Combined2 <- data.frame(rbind(Combined2, Joint))

# Better labels
Combined2$Separation <- factor(Combined2$Separation,
 levels = c("Sep. = 0", "Sep. = 0.5", "Sep. = 1", "Sep. = 2", "Sep. = 3",
 "Sep. = 4", "Sep. = 6", "Sep. = 8", "Sep. = 10", "Sep. = 12",
 "86th Senate", "109th House"))
Combined2 <- Combined2[!is.element(Combined2$Separation, c("Sep. = 8", "Sep. = 10", "Sep. = 12")), ]
Combined2$Party <- factor(Combined2$Party, levels = c("Combined", "Dem", "Rep"))

# Plot
ZP2d <- qplot(data = Combined2, x = x, y = y, geom = "polygon",
 group = Party, colour = Party, fill = Party, alpha = I(2/3))
ZP2d <- ZP2d + facet_wrap(~ Separation, scale = "free")
ZP2d <- ZP2d + scale_fill_manual(values = MyPalette(5)[c(3, 5, 5)])
ZP2d <- ZP2d + scale_colour_manual(values = MyPalette(5)[c(5, 1, 1)])
ZP2d <- ZP2d + theme(
 legend.position = "none",
 axis.text.x = element_blank(), axis.text.y = element_blank(),
 axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),
 panel.grid.minor = element_blank(),  panel.grid.major = element_blank(),
 panel.border = element_rect(colour = NA),
 strip.background = element_rect(fill = "grey80", colour = NA),
 plot.margin = unit(c(0, 0, -1, -1), "lines"))
ZP2d

# Save the plot
ggsave(plot = ZP2d, "~/Dropbox/Dimensionality/PAFinal/PaperFiles/TexFiles/Figure3.png", type = "cairo-png", h = 4.5, w = 6)
