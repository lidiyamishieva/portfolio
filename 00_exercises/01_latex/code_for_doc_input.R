#load package lattice
library(lattice)
library(xtable) # generate the LaTeX code for tables
#fix the random generator seed
set.seed(123)
#create data
data <- rnorm(1000)

# plot and export
png("histogram.png", width = 800, height = 600)
histogram(data)
dev.off()

# plot and export
png("density.png", width = 800, height = 600)
densityplot(data^12 / data^10, xlab = expression(data^12/data^10))
dev.off()

# plot and export
png("stripplot.png", width = 800, height = 600)
stripplot(data^2, xlab = expression(data^2))
dev.off()

# plot and export
png("boxplot.png", width = 800, height = 600)
bwplot(exp(data))
dev.off()

#matrix with all data used
data.all <- cbind(data = data, 
                  squared1 = data^12 / data^10,
                  squared2 = data^2,
                  exponent = exp(data))

# generate the LaTeX code for tables
xtable(data.all[1:9,])

