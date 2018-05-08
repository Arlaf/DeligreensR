library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

# Récupération des valeurs key et secret nécessaires àla connexion à l'API Shopify
source(file = "/home/arnaud/Documents/Linfo_spfy.R")

# Récupération des valeurs nécessaires à la BD Core
source(file = "/home/arnaud/Documents/Linfo_core.R")
dbname <- Sys.getenv("dbname")
dbhost <- Sys.getenv("dbhost")
dbuser <- Sys.getenv("dbuser")
dbpass <- Sys.getenv("dbpass")

# Fonctions
source(file = "../Fonctions_API_Shopify.R")
source(file = "../Fonctions_Core_DB.R")

####### Produits Shopify #######
# Importation du dataframe tax_overrides qui contient les id des collections de tax overrides et leur tax_rate
source(file = "../tax_overrides.R")

# Récupération de tous les produits actuels de shopify
prod <- get_products(key, secret) %>%
  rename(price_shopify = price) %>%
  mutate(tax_rate = get_taxes(key, secret, shopify_id, df_overrides))

####### Collections Shopify #######
# Valeurs brutes : identifiants et noms des collections
{
  # Collections "de vente"
  title <- c("Fruits & Légumes",
             "Crèmerie",
             "Viandes",
             "Poissonnerie",
             "Traiteur",
             "Boulangerie / Pâtisserie",
             "Epicerie Sucrée",
             "Epicerie Salée",
             "Boissons",
             "Cave",
             "Hygiène et Entretien",
             "Fleurs",
             "Bébés & Enfants")
  
  handle <- c("fruits-legumes",
              "produits-laitiers",
              "viandes",
              "poissonnerie",
              "traiteur",
              "patisserie-boulangerie",
              "epicerie-sucree",
              "epicerie-salee",
              "boissons",
              "cave",
              "hygiene-et-entretien",
              "fleurs",
              "bebes-enfants")
  
  collection_id <- c(399234954,
                     399241162,
                     28828333,
                     399239882,
                     13046186013,
                     399236298,
                     435241226,
                     435243530,
                     417670538,
                     416960842,
                     448607754,
                     33088274521,
                     448511946)

}

# Collection ventes
  # Tous les produits de la première collection dans un dataframe
  col_ventes <- data.frame(col_id = collection_id[1],
                    col_title = title[1],
                    col_handle = handle[1],
                    shopify_id = get_collection(key, secret, collection_id[1]))
  
  # Ajout des produits des autres collections
  for(i in 2:length(collection_id)){
    tmp <- data.frame(col_id = collection_id[i],
                      col_title = title[i],
                      col_handle = handle[i],
                      shopify_id = get_collection(key, secret, collection_id[i]))
    col_ventes <- rbind(col_ventes, tmp)
  }
  
####### Produits Core #######
req <- "SELECT 	p.id AS product_id, 
              	p.shopify_id,
              	p.title AS product_title,
              	p.handle AS product_handle,
              	p.product_type,
              	p.created_at AS product_created_at,
              	p.updated_at AS product_updated_at,
                v.shopify_id AS variant_id,
              	v.sku AS variant_sku,
                v.created_at AS variant_created_at,
                v.updated_at AS variant_updated_at,
              	v.buying_price_cents/100::float AS buying_price_ht,
              	v.price_cents/100::float AS selling_price_ttc
        FROM products p, variants v
        WHERE p.id = v.product_id AND "

# On ne demande que les produits qu'on trouve aussi dans l'API Shopify
liste_prod <- unique(prod$shopify_id)
where <- paste(" p.shopify_id in (", paste0(liste_prod, collapse = ","),")")
req <- paste(req, where)

# Extraction de core
core <- extract_core(req, dbname, dbhost, dbuser, dbpass)

# On ne garde que la dernière MAJ pour chaque variant
last_update <- core %>%
  group_by(shopify_id, variant_id) %>%
  summarise(variant_updated_at = max(variant_updated_at)) %>%
  ungroup()

# On ramène les données complète dans les variant filtrées
core <- left_join(last_update, core, by = c("shopify_id", "variant_id", "variant_updated_at"))

####### Rassemblement des données #######
# On regroupe les données Core et Shopify
df <- prod %>%
  select(shopify_id, variant_id, vendor, price_shopify, published, tax_rate) %>%
  left_join(core, by = c("shopify_id", "variant_id")) %>%
  left_join(col_ventes, by = "shopify_id")

df <- df %>%
  mutate(selling_price_ht = round(price_shopify/(1+tax_rate), 2),
         margin = selling_price_ht - buying_price_ht,
         margin_rate = round(100*margin/selling_price_ht, 1)) %>%
  # Ordre des colonnes
  select(shopify_id,
         product_id,
         product_title,
         product_handle,
         published,
         vendor,
         buying_price_ht,
         selling_price_ht,
         margin,
         margin_rate,
         tax_rate,
         price_shopify, # C'est le selling_price_ttc
         product_type,
         col_title,
         col_handle,
         col_id) %>%
  rename(product_id_shopify = shopify_id,
         product_id_core = product_id,
         collection_title = col_title,
         collection_handle = col_handle,
         collection_id = col_id,
         selling_price_ttc = price_shopify) %>%
  arrange(collection_title, product_type, product_handle)

name <- paste0("Export_Produits_", format(Sys.Date(), format = "%d-%m-%y"), ".csv")
write.table(df, file = paste0("/home/arnaud/Documents/", name), row.names = F, sep = ",", dec = ".")
