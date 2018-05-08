library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

# Transforme la réponse JSON d'une requête http en df (ou valeur unique le cas échéant)
from_rep_to_data <- function(reponse){
  
  content <- reponse$content %>%
    rawToChar() %>%
    fromJSON()
  sortie <- content[[1]]
  sortie
}

# Renvoie le nombre d'objet d'un endpoint Shopify donné
get_count <- function(key, secret, url, params = ""){
  
  # Authentificaton
  auth <- authenticate(key,secret, type = "basic")
  
  rep <- GET(url = paste0(url, params),
             add_headers(Content_Type = "application/json",
                         Accept = "application/json"),
             auth)
  
  N <- from_rep_to_data(rep)
  N
}

# Renvoie le vecteur des shopify_id de tous les produits d'une collection
get_collection <- function(key, secret, collection_id){
  # Renvoie le vecteur des product_id de la collection
  
  # Authentificaton
  auth <- authenticate(key,secret, type = "basic")
  
  url <- "https://courtcircuit.myshopify.com/admin/collects"
  
  # Récupération du nombre de produit dans la collection
  N <- get_count(key, secret, url = paste0(url,"/count"), params = paste0("?collection_id=", collection_id))
  
  # Calcul du nombre de requêtes nécessaires (250 items max par itération)
  n <- ceiling(N/250)
  
  # Exécution des requêtes
    # Première itération
    params <- paste0("?limit=250&collection_id=", collection_id,"&page=1")
    
    rep <- GET(url = paste0(url, params),
               add_headers(Content_Type = "application/json",
                           Accept = "application/json"),
               auth)
    df <- from_rep_to_data(rep)
    prod_ids <- df$product_id
    
    # Itération suivantes
    if(n > 1){
      for(i in 2:n){
        params <- paste0("?limit=250&collection_id=", collection_id,"&page=",i)
        
        rep <- GET(url = paste0(url, params),
                   add_headers(Content_Type = "application/json",
                               Accept = "application/json"),
                   auth)
        df <- from_rep_to_data(rep)
        prod_ids_temp <- df$product_id
        
        # Fusion avec les itération précédentes
        prod_ids <- c(prod_ids, prod_ids_temp)
      }
    }
    prod_ids
}

# Renvoie un dataframe qui contient l'intégralité des produits de Shopify (Avec possibilité de paramétrer la requête http)
get_products <- function(key, secret, params = ""){
  
  # On enlève l'éventuels ? ou & du début de chaine de params
  if(substr(params,1,1) %in% c("&","?")){
    params <- substr(params, 2, nchar(params))
  }
  
  # Authentificaton
  auth <- authenticate(key,secret, type = "basic")
  
  url <- "https://courtcircuit.myshopify.com/admin/products"
  
  # Récupération du nombre de produits
  N <- get_count(key, secret, url = paste0(url, "/count?", params))
  
  # Calcul du nombre de requêtes nécessaires (250 items max par itération)
  n <- ceiling(N/250)
  
  # Exécution des réquêtes
    # 1ère itération
    parametres <- paste0("?limit=250&page=1&", params)
    rep <- GET(url = paste0(url,parametres),
               add_headers(Content_Type = "application/json",
                           Accept = "application/json"),
               auth)
    df <- from_rep_to_data(rep) %>%
      select(-options, -images, -image, -body_html) %>%
      unnest()
    
    # itérations suivantes
    if(n > 1){
      for (i in 2:n){
        parametres <- paste0("?limit=250&page=", i, "&", params)
        rep <- GET(url = paste0(url,parametres),
                   add_headers(Content_Type = "application/json",
                               Accept = "application/json"),
                   auth)
        df_temp <- from_rep_to_data(rep) %>%
          select(-options, -images, -image, -body_html) %>% 
          unnest()
        df <- rbind(df, df_temp)
      }
    }
    
  df <- df %>%
    rename(shopify_id = id,
           variant_id = id1,
           variant_title = title1,
           variant_created_at = created_at1,
           variant_updated_at = updated_at1) %>%
    mutate(published = !is.na(published_at))
  df$price <- as.numeric(df$price)
  df
}

# Prend un vecteur d'identifiants produits shopify et renvoie un vecteur correspondant à leur taux de taxe
get_taxes <- function(key, secret, shopify_id, df_overrides){
  
  df <- data.frame(shopify_id = shopify_id)
  
  # On récupère tous les identifiants des produits qui font partie d'une collection tax override
      # Tous les produits de la première collection dans un dataframe
      col_tax <- data.frame(shopify_id = get_collection(key, secret, df_overrides$id[1]),
                            tax_rate = df_overrides$tax_rate[1])
      
      # Ajout des produits des autres collections
      for(i in nrow(df_overrides)){
        tmp <- data.frame(shopify_id = get_collection(key, secret, df_overrides$id[i]),
                          tax_rate = df_overrides$tax_rate[i])
        col_tax <- rbind(col_tax, tmp)
      }
      
  df <- left_join(df, col_tax, by = "shopify_id")
  
  # Pour tous les produits sans override on a des NA qu'on remplace par la taxe par défaut : 5,5%
  df$tax_rate[is.na(df$tax_rate)] <- 0.055
  
  df$tax_rate
}