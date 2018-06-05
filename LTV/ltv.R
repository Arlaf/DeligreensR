library(ggplot2)
library(dplyr)

source("../Fonctions_Core_DB.R")
source("~/Documents/Linfo_core.R")

# Informations de connexion à la DB Core
dbname <- Sys.getenv("dbname")
dbhost <- Sys.getenv("dbhost")
dbuser <- Sys.getenv("dbuser")
dbpass <- Sys.getenv("dbpass")

# Liste des équipiers
email_equipier <- c('dumontet.thibaut@gmail.com', 'dumontet.julie@gmail.com', 'laura.h.jalbert@gmail.com', 'rehmvincent@gmail.com', 'a.mechkar@gmail.com', 'helena.luber@gmail.com', 'martin.plancquaert@gmail.com', 'badieresoscar@gmail.com', 'steffina.tagoreraj@gmail.com', 'perono.jeremy@gmail.com', 'roger.virgil@gmail.com', 'boutiermorgane@gmail.com', 'idabmat@gmail.com', 'nadinelhubert@gmail.com', 'faure.remi@yahoo.fr', 'maxime.cisilin@gmail.com', 'voto.arthur@gmail.com')

# Récupération de la table clients
req <- "SELECT 	c.id AS client_id,
                c.email,
              	c.shopify_id,
              	c.orders_count,
              	c.created_at AS client_created_at,
              	c.first_order_date
        FROM clients c"
cli <- extract_core(req, dbname, dbhost, dbuser, dbpass) %>%
  # On enlève les équipiers puis la colonne email qui n'est plus utile
  filter(!(grepl("@deligreens.com$",email) | email %in% email_equipier)) %>%
  select(- email)

# Récupération de la table orders
req <- "SELECT  o.order_number,
              	o.client_id,
              	o.created_at AS order_created_at,
              	o.total_price_cents AS total_price,
              	SUM(CEILING(li.quantity * li.selling_price_cents/(1+tax_rate))) AS gross_sale,
              	o.pickup,
              	o.discount_code
  FROM orders o, line_items li
  WHERE o.id = li.order_id 
  GROUP BY  o.order_number,
          	o.client_id,
          	o.created_at,
          	o.total_price_cents,
          	o.pickup,
          	o.discount_code
  ORDER BY client_id, order_created_at"
com <- extract_core(req, dbname, dbhost, dbuser, dbpass)%>%
  # On ne garde que les commandes des clients qui ne sont pas des équipiers
  filter(client_id %in% cli$client_id)

######### Au bout de combien de temps peut-on considérer qu'un client n'est plus client ? #########

# On rajoute client_created_at, fisrt_order_date et order_count au DF des commandes
com <- cli %>%
  select(client_id, client_created_at, first_order_date, orders_count) %>%
  left_join(com, ., by = "client_id")

com <- com %>%
  # calcul du délai entre une commande et la précédente d'un client
  mutate(delai = ifelse(client_id == lag(client_id), round(difftime(order_created_at, lag(order_created_at), units = "days")), NA)) %>%
  # nieme = la commande est la combientième du client
  group_by(client_id) %>%
  mutate(nieme = row_number() + orders_count - max(row_number())) %>%
  # derniere : la commande est-elle la dernière du client
  mutate(derniere = ifelse(nieme == orders_count, T, F)) %>%
  ungroup() %>%
  # pas_revu_depuis : Combien de temps s'est-il écoulé depuis que le client n'a pas commandé
  mutate(pas_revu_depuis = ifelse(derniere, round(difftime(now(),order_created_at, units="days")),NA)) %>%
  # Conversion des centimes en €
  mutate(total_price = total_price/100,
         gross_sale = gross_sale/100)

# # On ne garde que les clients qui ont commandé au moins 2 fois
# com <- com %>%
#   filter(orders_count > 1)

# Création d'un dataframe : une ligne est un délai entre 2 commandes successives d'un client (revenu = TRUE) ou entre sa derniere commande et aujourd'hui (revenu = FALSE)
df <- data.frame(duree = com$delai[!is.na(com$delai)],
                 revenu = T)
df <- bind_rows(df, data.frame(duree = com$pas_revu_depuis[!is.na(com$pas_revu_depuis)],
                               revenu = F))

# Délai entre 2 commandes
q <- quantile(com$delai, probs = seq(0.9,1,0.005), na.rm = T)
print(q)

ggplot(mapping = aes(na.omit(com$delai))) +
  stat_ecdf(geom = "step") +
  geom_vline(xintercept = q["99%"], color = "red", size = 0.5) +
  geom_text(aes(x = q["99%"]*1.25, y = 0.95, label = paste("99 % des délais sont\n inférieurs à",round(q["99%"]), "jours")), color = "red") +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  scale_x_continuous(breaks = seq(0, max(com$delai, na.rm = T), 20)) +
  ggtitle("Courbe de répartition des délais entre 2 commandes") +
  xlab("X = Délai en jours") +
  ylab("Pourcentage de délais inférieurs à X") +
  theme_bw()

# fonction : Quand ça fait X jours que les clients n'ont pas commandé, combien sont revenus ?
revenu.apres <- function(df,x){
  return(round(sum(df$revenu[df$duree > x])/nrow(df[df$duree > x,]),3))
}

x <- 15:max(com$delai, na.rm = T)
y <- sapply(x, revenu.apres, df = df)
coord <- data.frame(x = x, y = y)

pct <- coord$y[coord$x == round(q["99%"])]
jours <- round(q["99%"])
label1 <- c(paste(pct*100, "%"),
          paste(jours, "jours"))
xlabel1 <- c(-13,
           q["99%"])
ylabel1 <- c(pct,
           -0.009)
label2 <- paste0("Seuls ", pct*100, " % des clients qui ont atteint\n", jours, " jours sans commander\nont fini par repasser une commande")
xlabel2 <- jours
ylabel2 <- pct

ggplot() +
  geom_point(data = coord, aes(x = x, y = y), size = 0.7) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_text(mapping = aes(x = xlabel1, y = ylabel1, label = label1), color = "red") +
  geom_text(mapping = aes(x = xlabel2, y = ylabel2, label = label2), color = "red", vjust = 0, hjust = 0) +
  geom_segment(aes(x = q["99%"], xend = q["99%"], y = 0, yend = coord$y[coord$x == round(q["99%"])]), linetype = "dashed", colour = "red") +
  geom_segment(aes(x = 0, xend = q["99%"], y = coord$y[coord$x == round(q["99%"])], yend = coord$y[coord$x == round(q["99%"])]), linetype = "dashed", colour = "red") +
  scale_y_continuous(breaks = seq(0,1,0.05)) +
  scale_x_continuous(breaks = seq(0, max(com$delai, na.rm = T), 20)) +
  ggtitle("La probabilité qu'un client passe une nouvelle commande selon la date de sa dernière commande") +
  xlab("Nombre de jours sans commande") +
  ylab("Pourcentage des clients qui ont passé une commande après X jours") +
  theme_bw()

####### TRAVAIL DE LA TABLE CLIENT #######   
# On ajoute au DF client la colonne pas_revu_depuis ainsi que la date de la dernière commande du client
cli <- com %>%
  filter(derniere) %>%
  select(client_id, order_created_at, pas_revu_depuis) %>%
  rename(lastest_order_date = order_created_at) %>%
  left_join(cli, ., by = "client_id")
  
# Calcul de l'âge du client : Durée entre sa première et dernière commande
cli <- cli %>%
  mutate(age = floor(difftime(lastest_order_date, first_order_date, units = "days")))

# Délai maximum entre 2 commandes d'un client : pour ça il faut au moins 2 commandes dans la base (certains clients ont un orders_counts > 1 mais comme elles datent d'avant le 30 avril 2017 ont les a pas en base)
cli <- com %>%
  group_by(client_id) %>%
  summarise(n = n(),
            delai_max = max(delai, na.rm = T)) %>%
  ungroup() %>%
  filter(n > 1) %>%
  select(-n) %>%
  left_join(cli, ., by = "client_id")

# pas_revu_depuis est NA -> deux cas identifiés : doublon de client ; première et seule commande annulée
# On retire donc ces cas du dataframe
cli <- cli %>%
  filter(!is.na(pas_revu_depuis))

# Création de la variable actif : le client est-il toujours client
seuil <- 150
cli <- cli %>%
  mutate(actif = !(pas_revu_depuis >= seuil | is.na(pas_revu_depuis)))

x <- 0:20
ggplot() +
  geom_point(aes(x = x, y = -x^2)) +
  geom_text(aes(x = 10, y = -100, label = "blablablablablablablablabla\nblablablablablablablablabla"), vjust = 1)

ggplot() +
  geom_point(aes(x = 1, y = 1)) +
  geom_text(aes(x = 1, y = 1, label = "blablablablablablablablabla\nblablablablablablablablabla"))
