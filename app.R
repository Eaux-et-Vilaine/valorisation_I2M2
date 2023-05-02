library(shiny)
library(DT)
library(devtools)
# ligne à exécuter seulement la première fois, peut être commentée après.
install_github("https://github.com/AnthonyDEBUR/tools4DCE")
library(tools4DCE)

# chargement de la liste des stations de référence de l'Armoricain
ref_staq_fr<-read.csv2("data/referentiel_fr.csv")
ref_staq_fr<-ref_staq_fr%>%subset(RRP=="OUI" & TYPEFR_CONSOLIDE%in%c("TP12-A", "P12-A", "M12-A", "G12-A", "TP12-B", "P12-B", "M12-B", "G12-B"))



# Définition de l'interface utilisateur (UI)
ui <- navbarPage(
  # Titre de la page
  title = "Valorisation données I2M2",
  
  # Onglet "Charger fichier de données"
  tabPanel("Charger fichier de données",
           # Zone de chargement de fichier
           fileInput("file", "Sélectionnez un fichier texte (.txt)"),
           
           # Bouton de soumission
           actionButton("submit", "Charger le fichier"),
           
           # Zone de sortie pour afficher les informations sur le fichier chargé
           verbatimTextOutput("info"),
           
           # Zone de sortie pour afficher le contenu du fichier
           dataTableOutput("table")
  ),
  # Onglet "Visualisation des résultats"
  tabPanel("Visualisation des résultats",
           selectInput("operation", 
                       "Sélectionner une opération", 
                       choices=NULL)
  ),
  # Onglet "Charger les références"
  tabPanel("Charger les références",
           # Titre de la section
           h2("Charger les données des stations de référence"),
           
           # Texte d'information
           p("Ce bouton permet de télécharger les données au niveau des stations de référence sur l'hydroécorégion massif armoricain avec l'API HUBEAU."),
           
           # Bouton de téléchargement des données de référence
           actionButton("download_refs", "Charger les données")
  ),
  
  # Onglet "A propos"
  tabPanel("A propos",
           # Texte d'information
           p("Cette application a été créée avec Shiny, un framework pour le développement d'applications web interactives en R.")
  )
)

# Définition du serveur (Server)
server <- function(input, output) {
  # Fonction de lecture de fichier
  read_file <- function(path) {
    # Lecture du fichier
    content <- read.delim(path, sep = "\t", header = TRUE)
    
    # Transformation du contenu du fichier
    content$CODE_OPERATION <- paste(content$CODE_STATION, content$DATE, content$CODE_PRODUCTEUR, sep = "*")
    
    # Retourne le contenu du fichier
    return(content)
  }
  
  # Réaction au clic sur le bouton "submit"
  observeEvent(input$submit, {
    # Vérification que le fichier a bien été sélectionné
    if(is.null(input$file)) {
      return(NULL)
    }
    
    # Lecture du fichier
    content <- read_file(input$file$datapath)
    
    # Stockage du contenu du fichier dans une variable globale
    assign("file_content", content, envir = .GlobalEnv)
    
    # Affichage des informations sur le fichier chargé
    output$info <- renderPrint({
      paste0("Fichier chargé :", input$file$name, "<br/>",
             "Nombre de lignes : ", nrow(content), "<br/>",
             "Nombre de colonnes : ", ncol(content), "<br/>")
    })
    
    # Affichage du contenu du fichier sous forme de tableau
    output$table <- renderDataTable({
      content
    }, options = list(searchable = TRUE, pageLength = 10))
  })
  
  # Réaction au clic sur le bouton "download_refs"
  observeEvent(input$download_refs, {
    # Charger les données indices invertébrés depuis hubeau pour totues les stations de référence 
    stations_op<-tools4DCE::import_hubeau_indices_hbio(liste_stations=ref_staq_fr$Code.SANDRE.de.la.station, 
                                            indice="inv")
    # on ne conserve que les I2M2
    stations_op<-stations_op%>%subset(CdParametre=="7613")
    
    # pour chacune des stations de référence, on charge les listes faunistiques et on exécute le script SEEE / outil diag
    for(i in 1:length(ref_staq_fr$Code.SANDRE.de.la.station))
    {
          # import des listes faunistiques
      donnees<-import_hubeau_liste_hbio(liste_stations = ref_staq_fr$Code.SANDRE.de.la.station[i], indice="inv")
    
      # mise en forme des listes faunistiques
    donnees<-donnees%>%subset(date_prelevement%in%stations_op$DatePrel)
    donnees$CODE_OPERATION <-
      paste0(donnees$code_station_hydrobio, "*", gsub("//","_",donnees$date_prelevement))
    donnees$CODE_STATION <- donnees$code_station_hydrobio
    donnees$DATE <- format(donnees$date_prelevement, "%d/%m/%Y")
    donnees$TYPO_NATIONALE <- "P12-A"
    donnees$CODE_PHASE <- donnees$code_lot
    donnees$CODE_TAXON <- donnees$code_appel_taxon
    donnees$RESULTAT <- donnees$resultat_taxon
    donnees$CODE_REMARQUE <- donnees$code_type_resultat
    donnees <- donnees %>% select(CODE_OPERATION, CODE_STATION, DATE, TYPO_NATIONALE, CODE_PHASE, CODE_TAXON, RESULTAT, CODE_REMARQUE)
    
    # Appel de l'outil diag du SEEE
    resultats_OD<-calcule_SEEE_ODinvertebres(donnees)
    
    # agrégation des fichiers de résultat et des listes faunistiques
    ifelse(i==1,
      donneesout<-donnees,
      donneesout<-rbind(donnees, donneesout))

    ifelse(i==1,
           ODout<-resultats_OD,
           ODout<-rbind(resultats_OD, ODout))
      }
    
    saveRDS(donneesout, "data/listes_fau_stations_ref.rds")   
    saveRDS(ODout, "data/od_stations_ref.rds")
    
  })
}


# Lancement de l'application
shinyApp(ui = ui, server = server)
