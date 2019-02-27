################################################################################
# Simple simulation comparing regression based on categorization, restricted
# cubic splines and frqctional polynomials
# Sven Knüppel
# 2019/Feb/27
################################################################################

library(rms)
library(mfp)


# Simulation model --------------------------------------------------------
set.seed(20190227)
N <- 500
x <- rnorm(N)
y <- x + x^2 + rnorm(N)
simdat <- data.frame(x = x, y = y)


# Categorize x ------------------------------------------------------------
simdat$cat <- cut(simdat$x,
                  c(min(simdat$x - 0.1),
                    quantile(simdat$x,
                             probs = c(0.10, 0.5, 0.90, 1))),
                  labels = c(1:4)
)


# Regression models -------------------------------------------------------
fit_cat <- lm(y ~ cat, data = simdat)
fit_nl <- lm(y ~ rcs(x,
                     quantile(simdat$x,
                              probs = c(0.10, 0.5, 0.90))),
             data = simdat)
fit_fp <- mfp(y ~ fp(x), data = simdat)

# add knots to get a lines add the knots
simdat <- rbind(
  simdat,
  c(x = quantile(simdat$x, probs = c(0.10)), y = NA, cat = 1),
  c(x = quantile(simdat$x, probs = c(0.10)), y = NA, cat = 2),
  c(x = quantile(simdat$x, probs = c(0.50)), y = NA, cat = 2),
  c(x = quantile(simdat$x, probs = c(0.50)), y = NA, cat = 3),
  c(x = quantile(simdat$x, probs = c(0.90)), y = NA, cat = 3)
)
simdat <- simdat[order(simdat$x), ]



# Create a nice plot and save as png file ---------------------------------
png("concat.png",
    height = 640, width = 640 * 1.618,
    pointsize = 18
)

# based on scatterhist by Ken Kleinman
# https://www.r-bloggers.com/example-8-41-scatterplot-with-marginal-histograms/
x <- simdat$x
y <- simdat$y
zones <- matrix(c(2, 0, 1, 3), ncol = 2, byrow = TRUE)
layout(zones, widths = c(4 / 5, 1 / 5), heights = c(1 / 5, 4 / 5))
xhist <- hist(x, plot = FALSE)
yhist <- hist(y, plot = FALSE)
top <- max(c(xhist$counts, yhist$counts))
par(mar = c(1, 1, 1.5, 1))
plot(simdat$x, simdat$y,
     col = "gray", pch = 19,
     main = "",
     xlab = "",
     ylab = "",
     axes = FALSE,
     frame.plot = TRUE)
axis(side = 1, at = simdat$x, labels = FALSE)
axis(side = 2, at = simdat$y, labels = FALSE)
abline(
  v = quantile(simdat$x,
               probs = c(0.10, 0.5, 0.90),
               type = 8),
  lty = "dashed", 
  col = "darkgray")
lines(simdat$x, predict(fit_cat, simdat), col = "red", lwd = 3)
lines(simdat$x, predict(fit_nl, simdat), col = "blue", lwd = 3)
lines(simdat$x, predict(fit_fp, simdat), col = "#FFBF00", lwd = 3)
legend(min(x, na.rm = TRUE) - 0.2, 
       max(y, na.rm = TRUE) + 0.55,
       c("Categorized variable (quartiles)",
         "Restricted cubic splines (3 knots)",
         expression(paste(
           "Fractional Polynomials (",
           p[1] == 0.5, ", ", p[2] == 3, ")"))),
       text.col = c("red", "blue", "#FFBF00"),
       col = c("red", "blue", "#FFBF00"), # cex = 0.8,
       lwd = 2, box.lty = 0, bg = "white")
barplot(xhist$counts,
        axes = FALSE, ylim = c(0, top), space = 0,
        main = expression(paste(
          "Simulation model:  ",
          f(x) == x + x^2 + epsilon, ",  ",
          epsilon, " ~ ", N(0, 1))))
par(mar = c(3, 0, 1, 1))
barplot(yhist$counts, 
        axes = FALSE, 
        xlim = c(0, top), 
        space = 0, 
        horiz = TRUE)
par(oma = c(0, 3, 0, 0))
dev.off()
