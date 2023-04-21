#' SEA
#'
#' @param data, dataframe at SIBER format 
#'
#' @return density plot with the posterior estimates of the ellipses for each group 
#' @export
#'
#' @examples sea(siber_data.df)


# Create lists of plotting arguments to be passed onwards to each 
# of the three plotting functions.
community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = 0.95, lty = 1, lwd = 2)
group.hull.args      <- list(lty = 2, col = "grey20")

# fit-mvn options for running jags
parms <- list()
parms$n.iter <- 2 * 10^4   # number of iterations to run the model for
parms$n.burnin <- 1 * 10^3 # discard the first set of values
parms$n.thin <- 10     # thin the posterior by this many
parms$n.chains <- 2        # run this many chains

# define the priors
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3


sea_z0492 <- function(data){

mydata_Z0492 <- subset(data, community=="2")

mydata_Z0492 <- SIBER::createSiberObject(mydata_Z0492) 

# fit the ellipses which uses an Inverse Wishart prior
# on the covariance matrix Sigma, and a vague normal prior on the 
# means. Fitting is via the JAGS method.
ellipses.posterior <- SIBER::siberMVN(mydata_Z0492, parms, priors)

## ----density-plots---
# The posterior estimates of the ellipses for each group can be used to
# calculate the SEA.B for each group.
SEA.B <- SIBER::siberEllipses(ellipses.posterior)

my_clrs <- matrix(c("#f9ecd7", "#eec788","#E4A33A",
                             "#fcd5ca" ,"#f9ab96", "#F67451",
                             "#dee8e6", "#bed1ce", "#94b3ae",
                             "#ddccd9" ,"#c4a6bc", "#AC80A0",
                             "#c2d2ea", "#a3bbdf", "#678FCB",
                             "#d2cbe2","#a698c6","#6B54A0"), nrow = 3, ncol = 6)
                             
SIBER::siberDensityPlot(SEA.B,
                 ylab = expression("Standard Ellipse Area" ('\u2030' ^2) ),
                 xticklabels= c("A.risso","A.olfersii","L.crocodilus","M.punctatum",
                                "N.kroeyeri","X.copei"),
                 bty = "L",
                 main = "370m",
                 font.axis=1,
                 las = 1,
                 ylim = c(0,2),
                 clr= my_clrs, 
                 lwd = 0)

# Add red x's for the ML estimated SEA-c
points(1:ncol(SEA.B), group.ML[3,], col="white", pch = "x", lwd = 3)


}

