library( ggplot2 )

debug <- TRUE
png <- TRUE
large <- FALSE
stormData <- NULL

loadData <- function() {
    
    # Conditional data loading: Pull from file...
    if ( is.null( stormData ) ) { 
        
        # Conditional dataset size: full, refreshed, data sets...
        if ( large ) {
            
            if ( debug ) print( "Using BIG-ASSED versions of stormData..." )
            stormData <<- read.csv( "../data/repdata-data-StormData.csv" )
            
        } else { # ...or smaller samples
            
            if ( debug ) print( "Using TOY versions of stormData..." )
            stormData <<- read.csv( "../data/stormDataSample.csv" )
        }
        
    } else { # or use cached copy
        
        if ( debug ) print( "Using cached versions of stormData..." )
    }
}
sampleData <- function() {
    
    loadData()
    set.seed( 2112 )
    ## grab random rows
    stormDataSample <<- stormData[ sample( nrow( stormData ), size = 10000, replace = F ), ]
    
    write.csv( stormDataSample, file = "../data/stormDataSample.csv" )
}