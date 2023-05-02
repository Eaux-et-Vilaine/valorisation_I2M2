library(shiny)
library(DT)
library(devtools)
# ligne à exécuter seulement la première fois, peut être commentée après.
install_github("https://github.com/AnthonyDEBUR/tools4DCE")
library(tools4DCE)
library(shinybusy)

# chargement de la liste des stations de référence de l'Armoricain
ref_staq_fr <- read.csv2("data/referentiel_fr.csv")
ref_staq_fr <-
  ref_staq_fr %>% subset(
    RRP == "OUI" &
      TYPEFR_CONSOLIDE %in% c(
        "TP12-A",
        "P12-A",
        "M12-A",
        "G12-A",
        "TP12-B",
        "P12-B",
        "M12-B",
        "G12-B"
      )
  )

# chargement des données de référence
stations_op <<- readRDS("data/notes_I2M2_ref.rds")
donneesout <<- readRDS("data/listes_fau_stations_ref.rds")
ODout <<- readRDS("data/od_stations_ref.rds")

# Définition de l'interface utilisateur (UI)
ui <- navbarPage(
  # Titre de la page
  title = "Valorisation données I2M2",
  
  # Onglet "Charger fichier de données"
  tabPanel(
    "Charger fichier de données",
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
    tabPanel(
    "Visualisation des résultats",
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
    selectInput(
      "operation",
      "Sélectionner une ou plusieurs opérations",
      choices = NULL,
      multiple = TRUE
    ),
    # Bouton de soumission
    actionButton("submit_operations", "Calculer les métriques"),
    selectInput(
      "type_indicateur",
      "Indicateur",
      choices = c(
        "I2M2",
        "IBGN",
        "TRANSVERSAL",
        "SUBSTRAT",
        "COURANT",
        "ALTITUDE",
        "TEMPERATURE",
        "NOURRITURE",
        "LOCOMOTION",
        "LONGITUDINAL",
        "TROPHIE",
        "SAPROBIE",
        "SALINITE",
        "PH",
        "ALIMENTATION",
        "REPRODUCTION",
        "RESPIRATION",
        "TAILLE",
        "LONGEVITE",
        "VOLTINISME",
        "STADE"),
      selected = "I2M2"
    ),
    checkboxGroupInput("TBE_BE", 
                       "Afficher les références en",
                       choices=c("BON ETAT", "TRES BON ETAT"),
                                 selected=c("BON ETAT", "TRES BON ETAT"))
  ),
  mainPanel(
    # Graphique
    plotOutput(outputId = "graph_invertebre")
    
  ))),
  # Onglet "Charger les références"
  tabPanel(
    "Charger les références",
    # Titre de la section
    h2("Charger les données des stations de référence"),
    
    # Texte d'information
    p(
      "Ce bouton permet de télécharger les données au niveau des stations de référence sur l'hydroécorégion massif armoricain avec l'API HUBEAU. ATTENTION LA MISE A JOUR DURE PLUSIEURS MINUTES."
    ),
    
    # Bouton de téléchargement des données de référence
    actionButton("download_refs", "Charger les données")
  ),
  
  # Onglet "A propos"
  tabPanel(
    "A propos",
    # Texte d'information
    p(
      "Cette application a été créée avec Shiny, un framework pour le développement d'applications web interactives en R."
    )
  )
)

# Définition du serveur (Server)
server <- function(input, output, session) {
  # valeurs de l'outil diag 
  values_OD<-reactiveValues(metriques=NULL,
                            I2M2=NULL,
                            IBG=NULL)
  
  
  # quand le bouton submit_operations est pressé, on appelle le SEEE pour calculer les métriques des opérations sélectionnées 
  observeEvent(input$submit_operations,{
    show_modal_spinner(text="Interrogation du SEEE / outil diagnostic")
    values_OD$metriques<-calcule_SEEE_ODinvertebres(file_content%>%
                                                      subset(CODE_OPERATION%in%input$operation))
   show_modal_spinner(text="Interrogation du SEEE / outil I2M2")
   values_OD$I2M2<-calcule_SEEE_I2M2(file_content%>%
                                       subset(CODE_OPERATION%in%input$operation)) 
   ##### PB ICI DANS CALCUL I2M2 #####
   show_modal_spinner(text="Interrogation du SEEE / outil IBG-DCE")
   values_OD$IBG<-calcule_SEEE_IBG_DCE(file_content%>%
                                       subset(CODE_OPERATION%in%input$operation)) 
   remove_modal_spinner()
    
  })
  
  # Fonction de lecture de fichier
  read_file <- function(path) {
    # Lecture du fichier
    content <- read.delim(path, sep = "\t", header = TRUE)
    
    # Transformation du contenu du fichier
    content$CODE_OPERATION <-
      paste(paste0("0", content$CODE_STATION),
            content$DATE,
            content$CODE_PRODUCTEUR,
            sep = "*")
    
    # mise à jour des codes opération dans le sélécteur de l'onglet visualisation
    updateSelectInput(session,
                      inputId = "operation",
                      choices = unique(content$CODE_OPERATION))
    
    # Retourne le contenu du fichier
    return(content)
  }
  
  # Réaction au clic sur le bouton "submit"
  observeEvent(input$submit, {
    # Vérification que le fichier a bien été sélectionné
    if (is.null(input$file)) {
      return(NULL)
    }
    
    # Lecture du fichier
    content <- read_file(input$file$datapath)
    
    # Stockage du contenu du fichier dans une variable globale
    assign("file_content", content, envir = .GlobalEnv)
    
    # Affichage des informations sur le fichier chargé
    output$info <- renderPrint({
      paste0(
        "Fichier chargé :",
        input$file$name,
        "<br/>",
        "Nombre de lignes : ",
        nrow(content),
        "<br/>",
        "Nombre de colonnes : ",
        ncol(content),
        "<br/>"
      )
    })
    
    # Affichage du contenu du fichier sous forme de tableau
    output$table <- renderDataTable({
      content
    }, options = list(searchable = TRUE, pageLength = 10))
  })
  
  # Réaction au clic sur le bouton "download_refs"
  observeEvent(input$download_refs, {
    show_modal_progress_line() #indicateur de chargement
    # Charger les données indices invertébrés depuis hubeau pour totues les stations de référence
    stations_op <<-
      tools4DCE::import_hubeau_indices_hbio(liste_stations = ref_staq_fr$Code.SANDRE.de.la.station,
                                            indice = "inv")  # mise en mémoire dans l'environnement global
    
    saveRDS(stations_op, "data/notes_I2M2_ref.rds")
    # on ne conserve que les I2M2
    stations_op <- stations_op %>% subset(CdParametre == "7613")
    
    # pour chacune des stations de référence, on charge les listes faunistiques et on exécute le script SEEE / outil diag
    for (i in 1:length(ref_staq_fr$Code.SANDRE.de.la.station))
    {
      print(i)
      update_modal_progress((i - 0.5) / length(ref_staq_fr$Code.SANDRE.de.la.station)) # update progress bar value
      # import des listes faunistiques
      donnees <-
        import_hubeau_liste_hbio(liste_stations = ref_staq_fr$Code.SANDRE.de.la.station[i],
                                 indice = "inv")
      
      # mise en forme des listes faunistiques
      donnees <-
        donnees %>% subset(date_prelevement %in% stations_op$DatePrel)
      donnees$CODE_OPERATION <-
        paste0(
          donnees$code_station_hydrobio,
          "*",
          gsub("//", "_", donnees$date_prelevement)
        )
      donnees$CODE_STATION <- donnees$code_station_hydrobio
      donnees$DATE <- format(donnees$date_prelevement, "%d/%m/%Y")
      donnees$TYPO_NATIONALE <- "P12-A"
      donnees$CODE_PHASE <- donnees$code_lot
      donnees$CODE_TAXON <- donnees$code_appel_taxon
      donnees$RESULTAT <- donnees$resultat_taxon
      donnees$CODE_REMARQUE <- donnees$code_type_resultat
      donnees <-
        donnees %>% select(
          CODE_OPERATION,
          CODE_STATION,
          DATE,
          TYPO_NATIONALE,
          CODE_PHASE,
          CODE_TAXON,
          RESULTAT,
          CODE_REMARQUE
        )
      
      update_modal_progress(i / length(ref_staq_fr$Code.SANDRE.de.la.station)) # update progress bar value
      # Appel de l'outil diag du SEEE
      resultats_OD <- calcule_SEEE_ODinvertebres(donnees)
      
      # agrégation des fichiers de résultat et des listes faunistiques
      ifelse(i == 1,
             donneesout <- donnees,
             donneesout <- rbind(donnees, donneesout))
      
      ifelse(i == 1,
             ODout <- resultats_OD,
             ODout <- rbind(resultats_OD, ODout))
    }
    
    # mise en mémoire dans l'environnement global
    donneesout <<- donneesout
    ODout <<- ODout
    
    saveRDS(donneesout, "data/listes_fau_stations_ref.rds")
    saveRDS(ODout, "data/od_stations_ref.rds")
    remove_modal_progress()
    
  })
}


# Lancement de l'application
shinyApp(ui = ui, server = server)
