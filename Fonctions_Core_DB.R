library(RPostgreSQL)
library(sqldf)
library(lubridate)
library(dplyr)

func_checked <- function(x){
  # Position des "true" dans la chaine x
  t <-gregexpr("true",x)[[1]]
  # Position des "false" dans la chaine x
  f <-gregexpr("false",x)[[1]]
  # création d'un vecteur de booléen
  sort(c(t,f)[c(t,f)>0]) %in% t
}

# Si le dataframe passé en paramètre contient des colonnes de POSIXct, la fonction lance la fonction correction_tz sur chacune d'entre elles
verif_tz <- function(df){
  
  n <- ncol(df)
  posix_found <- F
  i <- 1
  # Recherche de posix
  while(!posix_found & i <= n){
    if(is.POSIXct(df[,i])){
      posix_found <- T
    }
    i <- i + 1
  }
  # Correction si il y a des posix
  if(posix_found){
    # identification des colonnes de POSIXct
    cols <- sapply(df, is.POSIXct)
    # Si il y a plusieurs colonnes de POSIXct
    if(sum(cols) > 1){
      # Création d'un dataframe contenant les bonnes dates
      a <- df[,cols] %>%
        lapply(correction_tz) %>%
        as.data.frame()
      # Collage des dataframe
      df <- cbind(df[,!cols],a)
      # Si il n'y a qu'une seule colonne de POSIXct
    }else{
      df[,which(cols)] <- correction_tz(df[,which(cols)])
    }
    
  }
  df
}

# Prend un vecteur de date en UTC pour les convertir en CET / CEST
correction_tz <- function(x){
  
  x <- as.POSIXct(as.character(x),tz = "UTC")
  attr(x, "tzone") <- "Europe/Paris"
  x
}

# Renvoie le résultat d'un requête SQL à la BD Core dans un dataframe<
extract_core <- function(req, dbname, dbhost, dbuser, dbpass){

  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  # note that "con" will be used later in each connection to the database
  con <- dbConnect(drv, 
                   dbname = dbname,
                   host = dbhost, 
                   port = 5432,
                   user = dbuser, 
                   password = dbpass)
  options(sqldf.RPostgreSQL.user = dbuser, 
          sqldf.RPostgreSQL.password = dbpass,
          sqldf.RPostgreSQL.dbname = dbname,
          sqldf.RPostgreSQL.host = dbhost, 
          sqldf.RPostgreSQL.port = 5432)
  
  df <- sqldf(req)
  
  # close the connection
  dbDisconnect(con)
  dbUnloadDriver(drv)
  
  # Correction des Time Zones :
  # Les données sont stockées en UTC mais lues en CET/CEST par R
  df <- verif_tz(df)
  
  df
}
