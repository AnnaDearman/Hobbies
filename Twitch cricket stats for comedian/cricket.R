##### INITIAL STEPS #####

# Import libraries

library( ggplot2 )

# Set working directory

setwd( "redacted" )

# Read in stats compiler's Excel sheet (save as .csv first)

raw <- read.csv( "matches.csv", skip = 3 )

##### EXTRACT INDIVIDUAL CRICKET MATCH TABLES #####

# Tables contain cricket stats for each person.
# The format is:
# "Batsman" indicates the top left cell and "Extras" indicates the bottom left cell. 
# Use this to locate individual tables.
# Find the row numbers that begin and end with these words.

firstlines <- which( raw$ENGLAND.Match.1 %in% "Batsman" )
lastlines <- which( raw$ENGLAND.Match.1 %in% "Extras" )

# Make a list called "matches", i.e. cricket matches, to store a table for each match

matches <- list()

# Extract each match and add it to the table

for ( i in 1:length( firstlines ) ){
  if ( raw$ENGLAND.Match.1[ ( firstlines[ i ] + 1 ) ] != "" ){ # For populated tables only
    matches[[ i ]] <- data.frame( raw[ ( firstlines[ i ] + 1 ):( lastlines[ i ] -1 ), 2:6 ] ) # extract the table
    rownames( matches[[ i ]] ) <- raw$ENGLAND.Match.1[ ( firstlines[ i ] + 1 ):( lastlines[ i ] -1 ) ] # assign row names
    colnames( matches[[ i ]] ) <- raw[ firstlines[ i ], 2:6 ] # assign column names
  }
}

##### MAKE A TABLE FOR EACH PLAYER #####

# Extract player names from all matches and retain a vector of unique names

playernames <- vector()

for ( i in 1:length( matches ) ){
  for ( j in 1:length( rownames( matches[[ i ]] ) ) ){ # look at the rownames of each table - these are the players
    if ( !(rownames( matches[[ i ]])[ j ] %in% playernames ) ){ # only add if unique
      playernames[ length( playernames ) +1 ] <- rownames( matches[[ i ]] )[ j ]
    }
  }
}

# Make a list in which to store rows of stats from each match.
# These will be grouped by player, instead of by match.
# List items will be named by player.

players <- vector( mode = "list", length = length( playernames ) )

names( players ) <- playernames

for ( i in 1:length( playernames ) ){ # loop over players
  man <- playernames[ i ] # get one player name
  for ( j in 1:length( matches ) ){ # loop over matches
    pos <- which( rownames( matches[[ j ]] ) %in% man ) # get the position in the current match table for the current player
    if ( length( pos ) != 0 ){ # ignore any tables without that player
      row1 <- dim( players[[ man ]] )[ 1 ] # Find out how many rows we currently have for that player
      row2 <- dim( players[[ man ]] )[ 1 ] + 1 # Define the next available row
      if ( is.null( row1 ) ){ # If there isn't a row in our list yet
        players[[ man ]] <- matches[[ j ]][ man, ] # add this row to the list
      }
      if ( !is.null( row1 ) ){ # If there's already at least one row in our list
        players[[ man ]][ row2, ] <- matches[[ j ]][ man, ] # add it as the next row
    }
    }
  }
}

##### OUTPUT GRAPHS AND STATISTICS #####

# Create a dataframe in a loop and use this to output whatever you want
# e.g. a histogram for runs, balls, SR and FOW, to supplement the "average"
# (available from stat compiler)

for ( i in 1:length( players ) ){
  df <- data.frame( players[[ playernames[ i ] ]] )
  df$Runs <- as.numeric( df$Runs ) # Make items numeric
  df$Balls <- as.numeric( df$Balls )
  df$SR <- as.numeric( df$SR )
  df$FOW <- as.numeric( df$FOW )
  print( ggplot( df, aes( x = Runs ) )+ # Plot
          geom_histogram()+
          labs( title = playernames[ i ] )
      )
  print( ggplot( df, aes( x = Balls ) )+
          geom_histogram()+
          labs( title = playernames[ i ] )
  )
  print( ggplot( df, aes( x = SR ) )+
          geom_histogram()+
          labs( title = playernames[ i ] )
  )
  print( ggplot( df, aes( x = FOW ) )+
          geom_histogram()+
          labs( title = playernames[ i ] )
  )
}

# You could also plot these stats as a function of time, 
# i.e. match number (date not provided)