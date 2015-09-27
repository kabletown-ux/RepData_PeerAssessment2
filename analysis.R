library( ggplot2 )

debug <- TRUE
png <- TRUE
large <- FALSE
stormData <- NULL

## proper data loading would point to URL, so that everybody's starting w/ the same data source 
## This is *not* the same as the same data set
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
    
    if ( debug ) {
        print( system.time( loadData() ) )
    } else {
        loadData()
    }
    set.seed( 2112 )
    ## grab random rows
    stormDataSample <<- stormData[ sample( nrow( stormData ), size = 10000, replace = F ), ]
    
    write.csv( stormDataSample, file = "../data/stormDataSample.csv" )
}
cleanData <- function() {
    
    ## remove superfluous columns (this matters w/ large datasets!)
    ## deal w/ borked strings
    ## impute missing values
}
doFatalities <- function() {
    
    loadData()
    fatalities <- aggregate( stormData[ "FATALITIES" ], by = stormData[ c( "EVTYPE" ) ], FUN = sum, na.rm = TRUE )
    orderedFatalities <- fatalities[ order( -fatalities$FATALITIES ), ]
    orderedFatalities <- orderedFatalities[ which( orderedFatalities$FATALITIES > 0 ), ]
    print( orderedFatalities )
    ggplot( data = orderedFatalities, aes( x = orderedFatalities$EVTYPE, y = orderedFatalities$FATALITIES, fill = EVTYPE ) ) + 
        geom_bar( stat = "identity" ) +
        theme( axis.text.x = element_text( angle = 45, hjust = 1 ) ) +
        xlab( "Type of Event" ) +
        ylab("Fatality Count" ) + 
        ggtitle( "NOAA's Highest Fatality Counts" )
}
doInjuries <- function() {
    
    loadData()
    injuries <- aggregate( stormData[ "INJURIES" ], by = stormData[ c( "EVTYPE" ) ], FUN = sum, na.rm = TRUE )
    orderedInjuries <- injuries[ order( -injuries$INJURIES ), ]
    orderedInjuries <- orderedInjuries[ which( orderedInjuries$INJURIES > 0 ), ]
    print( orderedInjuries ) 

    ggplot( orderedInjuries, aes( x = EVTYPE, y = INJURIES, fill = EVTYPE ) ) + 
        geom_bar( stat="identity", position = "dodge" ) + 
        labs( x = "Type of Event", y = "Fatality Count" ) + 
        labs( title = "NOAA's Highest Injury Counts" ) +
        theme( axis.text.x = element_text( angle = 45, vjust = 1, hjust=1 ) )
}

doCropDamages <- function() {
    
    loadData()
    cropDamages <- aggregate( stormData[ "CROPDMG" ], by = stormData[ c( "EVTYPE" ) ], FUN = sum, na.rm = TRUE )
    orderedCropDamages <- cropDamages[ order( -cropDamages$CROPDMG ), ]
    # replace w/ top 10
    #orderedCropDamages <- orderedCropDamages[ which( orderedCropDamages$CROPDMG > 0 ), ]
    orderedCropDamages <- head( orderedCropDamages, 10 )
    print( orderedCropDamages ) 
    
    ggplot( orderedCropDamages, aes( x = EVTYPE, y = CROPDMG, fill = EVTYPE ) ) + 
        geom_bar( stat="identity", position = "dodge" ) + 
        labs( x = "Type of Event", y = "Crop Damage (Thousands of Dollars?)" ) + 
        labs( title = "NOAA's Largest Crop Damage" ) +
        theme( axis.text.x = element_text( angle = 45, vjust = 1, hjust=1 ) )
}

doPropertyDamages <- function() {
    
    loadData()
    propertyDamages <- aggregate( stormData[ "PROPDMG" ], by = stormData[ c( "EVTYPE" ) ], FUN = sum, na.rm = TRUE )
    orderedPropertyDamages <- propertyDamages[ order( -propertyDamages$PROPDMG ), ]
    # replace w/ top 10
    #orderedPropertyDamages <- orderedPropertyDamages[ which( orderedPropertyDamages$PROPDMG > 0 ), ]
    orderedPropertyDamages <- head( orderedPropertyDamages, 10 )
    print( orderedPropertyDamages ) 
    
    ggplot( orderedPropertyDamages, aes( x = EVTYPE, y = PROPDMG, fill = EVTYPE ) ) + 
        geom_bar( stat="identity", position = "dodge" ) + 
        labs( x = "Type of Event", y = "Crop Damage (Thousands of Dollars?)" ) + 
        labs( title = "NOAA's Largest Crop Damage" ) +
        theme( axis.text.x = element_text( angle = 45, vjust = 1, hjust=1 ) )
}

