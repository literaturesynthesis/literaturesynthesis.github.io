# formation méta-analyse
# Auteur : Beillouin Damien
# Cours : Statistiques Descriptives
# Objectif du TD : Explorer et visualiser les données à l'aide de R

#### I/ Initialisation ####

#--------------------------------------------------------------
#### GRAPHIQUE 1:  Histogramme du nombre de données par catégories. 
#--------------------------------------------------------------

# Chargement des packages nécessaires
library("DIZutils")  # Pour la gestion des données
library(tidyr)       # Pour la gestion des données
library(cowplot)     # Pour les mises en page de graphiques
library(magrittr)    # Pour les opérateurs de pipeline
library(ggplot2)     # Pour les graphiques
library(ggpubr)      # Pour créer de beaux graphiques
library(colourpicker) # pour choisir les couleurs


# Choisir le fichier CSV nommé Data_for_histogram.csv dans votre ordinateur.
csv_file <- file.choose()

# Chargement des données à partir du fichier CSV choisi 
DATA <- read.csv(csv_file, sep=",")

#Jeu de données d'une evidence map des études qui analysent 
#l'impact des interventions humaines sur les niveaux de carbone
#dans les sols. Il se divise en trois catégories d'interventions :
#"global intervention" 
#(intervention globale), "management" (gestion), et "land-use 
#change" (changement d'utilisation des terres). Les résultats 
#peuvent concerner soit le carbone ("carbon") soit le carbone 
#et d'autres aspects ("carbon and other outcomes"). "LU" 
#précise des détails pour le changement d'utilisation des 
#terres, "ORDER" est un numéro d'ordre, "name" indique le 
#niveau de détail, et "value" donne le nombre d'informations
#disponibles pour chaque combinaison.

## Nous allons faire un graphique composé de sous plot, pour apporter
#de l'information sur le nombre de méta-analyses, nombre de primary studies,
# et nombre d'effect sizes

# Définir une palette de couleurs personnalisée
# on a 6 couleurs à définir dans notre cas
if (interactive()) {
  cols <- colourPicker(6)
}

# Créer une fonction pour le thème personnalisé
custom_theme <- function() {
  theme_pubr() + 
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
      axis.text = element_text(size = 12),
      axis.title.y = element_text(size = 14, vjust = 1),
      panel.grid.major = element_line(colour = "gray80", size = 0.2)
    )#+
    #theme(legend.position = "none")
}

# on fait le sous- graphique 1
# on sélectionne dans le fichier les lignes qui correspondent aux méta-analyses
DATA_MA<- DATA %>% dplyr::filter(name == "MA", outcome == "all")

# Créer un graphique ggplot
PLOT1 <-ggplot(DATA_MA) +
  # Définir les barres
  geom_bar(aes(x = reorder(int, value), y = value, group = LU, fill = LU), stat = "identity",
           position = "dodge2") +
  # Inverser l'axe des x (pour des barres horizontales)
  coord_flip() +
  # Appliquer un thème defini
  custom_theme() +
  # Ajouter une bordure au panneau du graphique
  labs(x = "", y = "Nombre de méta-analyses") +
  # Utiliser la palette de couleurs personnalisée
  scale_fill_manual(values = cols)+
# Changez "Catégorie" en ce que vous souhaitez afficher
  guides(fill = guide_legend(title = "Type de sol:"))  +
  theme(legend.position = c(0.65, 0.55)) 
PLOT1


# on fait le sous- graphique 2
# on sélectionne dans le fichier les lignes qui correspondent aux méta-analyses
DATA_ES<- DATA %>% dplyr::filter(name == "ES", outcome == "all")

# Créer un graphique ggplot
PLOT2<-ggplot(DATA_ES) +
  # Définir les barres
  geom_bar(aes(x = reorder(int, value), y = value, group = LU, fill = LU), stat = "identity",
           position = "dodge2") +
  # Inverser l'axe des x (pour des barres horizontales)
  coord_flip() +
  # Appliquer un thème defini
  custom_theme() +
  # Ajouter une bordure au panneau du graphique
  labs(x = "", y = "Nombre d'effect-size") +
  # Utiliser la palette de couleurs personnalisée
  scale_fill_manual(values = cols)+
  # Supprimer la légende
  theme(legend.position = "none") +
  # on supprime les légende de l'axe des y
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
PLOT2

# Créer une mise en page avec les trois graphiques côte à côte
plot_grid(PLOT1, PLOT2, ncol = 2, rel_widths = c(2, 1))



#--------------------------------------------------------------
#### GRAPHIQUE 2:  Carte du monde de la distribution des études 
#--------------------------------------------------------------

# Chargement des packages pour la création de la carte
library(rnaturalearth) # Pour la carte du monde
library(sf)            # Pour les données spatiales
library(dplyr)         # Pour gérer les données

# Chargement des données sur les pays
# Choisir le fichier CSV nommé Données Dataviz.csv dans votre ordinateur.
csv_file <- file.choose()

# Chargement des données à partir du fichier CSV choisi 
DATA <- read.csv(csv_file, sep= ';') %>%
  dplyr::mutate(Country = factor(tolower(Country))) %>%
  dplyr::rename(geounit = Country)
summary(DATA)
##  Cette base de données présente le nombre d'étude pour chacun des pays

# Chargement de la carte du monde
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(geounit = tolower(geounit)) %>%
  # Renomme le nom de certains pays
  mutate(geounit = dplyr::case_when(
    geounit == "united kingdom" ~ "uk",
    geounit == "united states of america" ~ "usa",
    TRUE ~ geounit
  ))

# Fusionner la carte et les données sur les pays
world2 <- merge(world, DATA, by = "geounit", all = TRUE) %>%
# Créer une variable discrète pour la légende
  mutate(cut_n = cut(Count, breaks = c(0, 10, 40, 80, 500)))

world2$cut_n <- cut(world2$Count, breaks = c(0, 10, 40, 80, 500))

# Créer la carte du noùbre de publication
map_plot <- ggplot(world2) +
  geom_sf(aes(fill = cut_n), size = 0.2, color = "gray20") +     
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(fill = 'Nombre d\'expériences',
       color = '',
       title = '',
       x = NULL,
       y = NULL) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = 'grey50', size = 0.3, linetype = 3)) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c('#f6e8c3',"#dfc283",'#ba966c','#e5f5e0','#a1d99b','#31a354'), na.value = "white")

map_plot

#--------------------------------------------------------------
#### GRAPHIQUE 3:  Création de la Treemap des Populations et Expositions
#--------------------------------------------------------------

# Chargement du package pour la création de la treemap
library(treemapify)


# Choisir le fichier CSV nommé Données Données Dataviz_pop.csv dans votre ordinateur.
csv_file <- file.choose()

# Chargement des données à partir du fichier CSV choisi 
DATA <- read.csv(csv_file, sep = ";")

# Créez un graphique treemap amélioré
treemap_plot <- ggplot(DATA, aes(area = Abstract, fill = Type, label = Type)) +
  geom_treemap() +
  # Ajoutez des bordures aux rectangles et définissez la couleur de la bordure
  geom_treemap_text(
    aes(label = sprintf("%s\n%d", Type, Abstract)), 
    size = 12,  # Ajustez la taille des étiquettes
    color = "white", 
    place = "centre"
  ) +
  # Ajoutez une légende pour la variable "Type"
  scale_fill_brewer(palette = "Set3") +
  labs(fill = "Type") +
  # Définissez un thème pour le graphique
  theme_minimal() +
  theme(legend.position = "bottom",  # Déplacez la légende en bas
        legend.title = element_blank(),  # Supprimez le titre de la légende
        plot.title = element_text(hjust = 0.5),  # Centrez le titre
        plot.subtitle = element_text(hjust = 0.5)) +  # Centrez le sous-titre
  
  # Ajoutez un titre et un sous-titre
  labs(
    title = "Répartition des données par type",
    subtitle = "Taille des rectangles basée sur la variable 'Abstract'"
  )

# Affichez le graphique
treemap_plot

#--------------------------------------------------------------
#### GRAPHIQUE 4:  Création de la Heatmap
#--------------------------------------------------------------

# Restructuration des données pour la heatmap
DATA %<>%  pivot_longer(c(`Abstract`, `Industrial`, 'Transportation', 'Military', 'Urban',
                         'Recreation', 'Other'))

# Palette de couleurs personnalisée
# on a 5 couleurs à définir dans notre cas
if (interactive()) {
  cols <- colourPicker(5)
}
# Création de la heatmap
heatmap_plot <- ggplot(DATA, aes(x = reorder(Type, -value), y = reorder(name, -value))) +
  geom_tile(aes(fill = cut(value, breaks = c(0, 10, 20, 30, 750)))) +
  geom_text(aes(label = round(value, 1)), size = 4, color = "black") +
  # définir les couleurs
  scale_fill_manual(
    values = cols,
    name = "Valeur",
    labels = c("< 10", "10 - 20", "20 - 30", "30 - >50","NA")
  ) +
  theme_minimal() +
  # Améliorer le thème du graphique
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title = element_blank()
  ) +
  labs(
    title = "Heatmap des valeurs par Type et name",
    fill = "Valeur"
  )

heatmap_plot




#--------------------------------------------------------------
#### GRAPHIQUE 5:  Création du bubble plot
#--------------------------------------------------------------

## VII/ Création de la Bubble Chart des Abondances Relatives ##
library(ggplot2)
library(ggpubr)

# Créez un graphique à bulles
bubble_plot <- ggplot(DATA, aes(x = Type, y = name)) +
  geom_point(aes(size = value, fill = value, color = value), alpha = 0.75, shape = 21) + 
  # Échelle personnalisée pour la taille des bulles
  scale_size_continuous(
    limits = c(0.000001, 100),  # Limites personnalisées
    range = c(3, 15),           # Plage de tailles personnalisées
    breaks = c(1, 10, 50, 75),  # Points de rupture pour les étiquettes de taille
    labels = c("1%", "10%", "50%", "75%")  # Étiquettes pour les tailles
  ) + 
  # Personnalisez les légendes
  labs(
    x = "",
    y = "",
    size = "Abondance relative (%)",
    fill = "Valeur",
    color = "Valeur"
  ) + 
  # Personnalisez les thèmes pour une meilleure lisibilité
  theme_minimal() +
  theme(
    legend.position = "right",  # Position de la légende
    legend.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(colour = "black", size = 10, angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(colour = "black", face = "bold", size = 10),
    legend.text = element_text(size = 10, face = "bold", colour = "black"),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1.2)
  ) +
  
  # Personnalisez la palette de couleurs
  scale_fill_gradient(low = "#F1EEF6", high = "#54278F") +
  scale_color_gradient(low = "#F1EEF6", high = "#54278F")

# Affichez le graphique à bulles
print(bubble_plot)

# possible de faire une version interactive
ggplotly(bubble_plot)


#--------------------------------------------------------------
#### GRAPHIQUE 6:  Création d'une table dynamique
#--------------------------------------------------------------

# on charge le package
library(reactable)

# Créez un résumé des données groupées
GROUP <- DATA %>%
  group_by(Type) %>%
  summarize(Number = n())

# Créez une réactable pour afficher les groupes et les détails
reactable(
  GROUP,
  details = function(index) {
    # Filtrer les détails pour le groupe sélectionné
    details_data <- filter(DATA, Type == GROUP$Type[index])
    
    # Créez une réactable pour les détails
    tbl <- reactable(details_data, outlined = TRUE, highlight = TRUE, fullWidth = TRUE,
                     columns = list(
                       value = colDef(
                         cell = function(value) {
                           if (value >= 0) paste0("+", value) else as.character(value)
                         },
                         style = function(value) {
                           color <- if (value > 10) {
                             "#008000"
                           } else if (value < 10) {
                             "#e00000"
                           }
                           list(fontWeight = 600, color = color)
                         }
                       )
                     ),
                     rowStyle = function(index) {
                       if (is.na(details_data[index, "value"])) {
                         list(background = "rgba(0, 0, 0, 0.05)")
                       }
                     }
    )
    htmltools::div(style = list(margin = "12px 45px"), tbl)
  },
  onClick = "expand",  # Définissez le comportement de clic pour étendre les détails
  rowStyle = list(cursor = "pointer")  # Définissez le style du curseur de la ligne
)


