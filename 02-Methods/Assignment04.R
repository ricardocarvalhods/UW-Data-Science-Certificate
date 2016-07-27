
# Analysis of Categorical Variables of Auto Data
# Ricardo S. Carvalho
# July 25th, 2016

##############################################################################################
## FUNCTIONS CREATED

## Read the csv file into a data frame
read.auto <- function(path = 'SET-YOUR-PATH-HERE'){
    ## Function to read the csv file
    filePath <- file.path(path, 'Automobile price data _Raw_.csv')
    auto.price <- read.csv(filePath, header = TRUE, 
                           stringsAsFactors = TRUE, na.strings = "?")
    
    ## Coerce some character columns to numeric
    numcols <- c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm',
                 'highway.mpg', 'city.mpg', 'compression.ratio',
                 'engine.size', 'curb.weight', 'height', 'width',
                 'length', 'wheel.base', 'normalized.losses',
                 'symboling')
    auto.price[, numcols] <- lapply(auto.price[, numcols], as.numeric)
    
    ## Clean and tidy num.of.doors
    auto.price$num.of.doors <- as.character(auto.price$num.of.doors)
    auto.price$num.of.doors[auto.price$num.of.doors == 'four'] <- 4
    auto.price$num.of.doors[auto.price$num.of.doors == 'two'] <- 2
    auto.price$num.of.doors <- as.integer(auto.price$num.of.doors)
    
    ## Since the only NAs in num.of.doors are of cars with body.style sedan, they have 4 doors
    auto.price$num.of.doors[is.na(auto.price$num.of.doors)] <- 4
    
    ## Clean and tidy num.of.cylinders
    auto.price$num.of.cylinders <- as.character(auto.price$num.of.cylinders)
    auto.price$num.of.cylinders[auto.price$num.of.cylinders == 'eight'] <- 8
    auto.price$num.of.cylinders[auto.price$num.of.cylinders == 'five'] <- 5
    auto.price$num.of.cylinders[auto.price$num.of.cylinders == 'four'] <- 4
    auto.price$num.of.cylinders[auto.price$num.of.cylinders == 'six'] <- 6
    auto.price$num.of.cylinders[auto.price$num.of.cylinders == 'three'] <- 3
    auto.price$num.of.cylinders[auto.price$num.of.cylinders == 'twelve'] <- 12
    auto.price$num.of.cylinders[auto.price$num.of.cylinders == 'two'] <- 2
    auto.price$num.of.cylinders <- as.integer(auto.price$num.of.cylinders)
    
    auto.price$lnprice <- log(auto.price$price)
    
    auto.price
}

# Adjusts data to remain only name.of.column (with the values given by values.of.column) and lnprice
redefine.data <- function(name.of.column, values.of.column){
    # Subsets rows and cols
    new.Auto <- Auto.Price[Auto.Price[, name.of.column] %in% values.of.column, c(name.of.column, 'lnprice')]
    new.Auto <- new.Auto[complete.cases(new.Auto),] # Keep only non NA values
    new.Auto[,name.of.column] <- factor(new.Auto[,name.of.column]) # Transforms to factor for ANOVA tests
    new.Auto
}

# Does ANOVA and Tukey's Range Test, outputing results in text and plot for Tukey's
anova.tests <- function(formula.to.anova, data.to.anova, leftpar=4){
    aov <- aov(formula.to.anova, data = data.to.anova) # Calculates ANOVA
    # Print ANOVA
    cat("  ANOVA\n")
    print(summary(aov))
    cat("\n----------------------------------------------------------------\n\n")
    tukey_anova <- TukeyHSD(aov) # Calculates Tukey's
    # Print Tukey's
    print(tukey_anova)
    par(mar=c(5,leftpar,4,2) + 0.1)
    plot(tukey_anova, las=1)
    par(mar=c(5,4,4,2) + 0.1)
}

# Outputs mean, lower and upper values of confidence interval constructed using bootstrap samples
bootstrap.diff.in.means <- function(main.col, col.to.boot, p=0.05, nr.iter=100000){
    library(simpleboot)
    col.to.boot <- factor(col.to.boot) # Vector needs to be factor
    categories <- unique(col.to.boot) # Obtain the categories in which to iterate
    boot.Output <- data.frame(diff=numeric(0), ci_lwr=numeric(0), ci_upr=numeric(0)) # Data frame with answers
    for(i in 1:(length(categories)-1)){
        for(j in (i+1):length(categories)){
            two.boot.mean = two.boot(main.col[col.to.boot == categories[j]], 
                                     main.col[col.to.boot == categories[i]], mean, R=nr.iter) # Bootstrap samples
            boot.values <- two.boot.mean$t
            # Calculates confidence intervals
            ci_lwr = round(quantile(boot.values, probs = p/2, na.rm=TRUE), 4)
            mean_vl = round(mean(boot.values), 4)
            ci_upr = round(quantile(boot.values, probs = (1 - p/2), na.rm=TRUE), 4)
            # Adds to data frame with answers
            current.boot = data.frame(diff=mean_vl, ci_lwr=ci_lwr, ci_upr=ci_upr)
            rownames(current.boot) <- paste0(categories[j], "-", categories[i])
            boot.Output <- rbind(boot.Output, current.boot)
        }
    }
    boot.Output
}

##############################################################################################
## Loading the data

Auto.Price = read.auto(path = '.')

##############################################################################################
## Body Style

table(Auto.Price$body.style, useNA = 'always')

body.style <- redefine.data('body.style', c('hatchback', 'sedan','wagon'))
anova.tests(formula(lnprice ~ body.style), body.style, 8)

bootstrap.diff.in.means(body.style$lnprice, body.style$body.style)

##############################################################################################
## Drive Wheels

table(Auto.Price$drive.wheels, useNA = 'always')

drive.wheels <- redefine.data('drive.wheels', c('rwd', 'fwd'))
sort(tapply(drive.wheels$lnprice, drive.wheels$drive.wheels, mean, na.rm=TRUE), decreasing=TRUE)

t.test(drive.wheels$lnprice[drive.wheels$drive.wheels == 'rwd'], 
       drive.wheels$lnprice[drive.wheels$drive.wheels == 'fwd'], "greater", 0, FALSE, FALSE, 0.95)

bootstrap.diff.in.means(drive.wheels$lnprice, drive.wheels$drive.wheels)

##############################################################################################
## Number of Cylinders

table(Auto.Price$num.of.cylinders, useNA = 'always')

cylinders <- redefine.data('num.of.cylinders', c(4,5,6))
anova.tests(formula(lnprice ~ num.of.cylinders), cylinders)

bootstrap.diff.in.means(cylinders$lnprice, cylinders$num.of.cylinders)

##############################################################################################
## Number of Doors

doors <- redefine.data('num.of.doors', c(2, 4))
sort(tapply(doors$lnprice, doors$num.of.doors, mean, na.rm=TRUE), decreasing=TRUE)

t.test(doors$lnprice[doors$num.of.doors == '4'], 
doors$lnprice[doors$num.of.doors == '2'], "two.sided", 0, FALSE, FALSE, 0.95)

bootstrap.diff.in.means(doors$lnprice, doors$num.of.doors)

##############################################################################################
## Engine Type

table(Auto.Price$engine.type, useNA = 'always')

engine.type <- redefine.data('engine.type', c('dohc','l','ohc','ohcf','ohcv'))x
anova.tests(formula(lnprice ~ engine.type), engine.type, 6)

bootstrap.diff.in.means(engine.type$lnprice, engine.type$engine.type)

