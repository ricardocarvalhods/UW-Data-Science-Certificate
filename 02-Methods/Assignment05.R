##  Read the csv file into a data frame
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


## read.auto function loads and cleans the data
Auto.Price = read.auto(path = '.')

## Creates linear model
lm.auto = lm(lnprice ~ engine.size + curb.weight + city.mpg, data = Auto.Price)

## Model summary
summary(lm.auto)

## Diagnostic plots
plot(lm.auto)

## Shapiro-Wilk normality test
shapiro.test(residuals(lm.auto))




