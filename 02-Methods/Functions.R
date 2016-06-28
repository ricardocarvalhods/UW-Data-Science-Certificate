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
    
    auto.price
}

# Automatically obtain numeric-like or character-like columns of data
getColsOfClass <- function(full.data, class='num'){
    if(class=='num'){
        classesChosen <- c('integer','numeric','double','float')
    }
    else{
        classesChosen <- c('factor','character')
    }
    return(sort(unique(sapply(1:ncol(full.data), 
                              function(x){
                                  if(class(full.data[,x]) %in% classesChosen){
                                      x
                                  }
                                  else{
                                      0
                                  }
                              }
    )
    )
    )[-1]
    )
}

# Obtain three highest correlated (absolute correlation) features of Auto.Price
featCorPrice <- function(price.function=I, n=3){
    cor.mat <- cor(Auto.Price[,numerical.cols], price.function(Auto.Price[,'price']), use = 'complete.obs')
    feat.cor <- abs(cor.mat)
    feat.cor <- feat.cor[order(feat.cor, decreasing = T),][-1]
    feat.cor[1:n]
}
