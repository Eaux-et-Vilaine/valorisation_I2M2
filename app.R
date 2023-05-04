library(shiny)
library(DT)
library(devtools)
# ligne à exécuter seulement la première fois, peut être commentée après.
install_github("https://github.com/AnthonyDEBUR/tools4DCE")
library(tools4DCE)
library(shinybusy)
library(cowplot)

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
ref_listes_fau <<- readRDS("data/listes_fau_stations_ref.rds")
ref_OD <<- readRDS("data/od_stations_ref.rds")
ref_I2M2<<- readRDS("data/i2m2_stations_ref.rds")
ref_IBGN<<- readRDS("data/ibgn_stations_ref.rds")

# fonction graphique

graphInv<-function(type, BE=TRUE, TBE=TRUE, dataI2M2, dataIBGN, dataOD){
  # type = type de graph
  # BE : booléen, TRUE pour afficher les résultats des références en bon état
  # TBE : booléen, TRUE pour afficher les résultats des références en très bon état
  # dataI2M2, dataIBGN et dataOD : données calculées avec le SEEE pour les opérations à afficher
  
  if(BE){
    # on ne conserve que les opérations dont l'I2M2 est bon état 
    oper_a_garder_BE<-ref_I2M2[ref_I2M2$CODE_PAR=="7613" & ref_I2M2$RESULTAT>=0.443,]$CODE_OPERATION
    oper_a_garder_BE<-oper_a_garder_BE[!is.na(oper_a_garder_BE)]%>%unique
    }else{oper_a_garder_BE<-NULL}
  
  
  if(TBE){
    # on ne conserve que les opérations dont l'I2M2 est bon état 
    oper_a_garder_TBE<-ref_I2M2[ref_I2M2$CODE_PAR=="7613" & ref_I2M2$RESULTAT>=0.665,]$CODE_OPERATION
    oper_a_garder_TBE<-oper_a_garder_TBE[!is.na(oper_a_garder_TBE)]%>%unique
    }else{oper_a_garder_TBE<-NULL}
  
  
  # graph I2M2
  if(type=="I2M2"){
    param<-data.frame(CdParametre=c("7613","8054", "8055", "8056", "8057", "8058"),
                      NomParametre=factor(c("I2M2","Rich. taxo", "Ovovivipar.", "Polyvolt.", "ASPT", "Shannon"), 
                                          levels=c("I2M2","Shannon", "Rich. taxo", "Ovovivipar.", "Polyvolt.", "ASPT"), 
                                          ordered=TRUE))
    
    ref_I2M2_BE<-ref_I2M2%>%subset(CODE_OPERATION%in%oper_a_garder_BE & CODE_PAR%in%param$CdParametre)
    ref_I2M2_BE<-left_join(ref_I2M2_BE, param, by=c("CODE_PAR"="CdParametre"))
    ref_I2M2_TBE<-ref_I2M2%>%subset(CODE_OPERATION%in%oper_a_garder_TBE & CODE_PAR%in%param$CdParametre)
    ref_I2M2_TBE<-left_join(ref_I2M2_TBE, param, by=c("CODE_PAR"="CdParametre"))
    
    data_graph<-dataI2M2%>%subset(CODE_PAR%in%param$CdParametre)
    data_graph<-left_join(data_graph, param, by=c("CODE_PAR"="CdParametre"))
    data_graph$DATE<-as.Date(data_graph$DATE, format="%d/%m/%Y")
    lbl_dates<-unique(paste(data_graph$CODE_STATION, format(data_graph$DATE, "%d/%m/%y")))
    data_graph$lbl_date<-factor(paste(data_graph$CODE_STATION, format(data_graph$DATE, "%d/%m/%y")), 
                                labels=lbl_dates,
                                ordered=TRUE) 
    
    
    ggplot(data_graph, aes(NomParametre, RESULTAT)) + 
      geom_violin(data=ref_I2M2_BE,aes(NomParametre, RESULTAT), fill="green")+ 
      geom_violin(data=ref_I2M2_TBE,aes(NomParametre, RESULTAT), fill="cyan")+ 
      geom_point() + 
      theme(axis.text.x = element_text(angle = 90)) + ylim(c(0,1)) + xlab("") +
      facet_grid(lbl_date~.) + ylab("")  
    }
  
 # graph IBGN

  if(type=="IBGN"){
    param<-data.frame(CdParametre=c( "5910", "6035", "6034"),
                      NomParametre=factor(c("IBG eq.", "GFI IBG", "Var. taxo. IBG"), 
                                          levels=c("IBG eq.", "GFI IBG", "Var. taxo. IBG"), 
                                          ordered=TRUE))
    
    ref_I2M2_BE<-ref_IBGN%>%subset(CODE_OPERATION%in%oper_a_garder_BE & CODE_PAR%in%param$CdParametre)
    ref_I2M2_BE<-left_join(ref_I2M2_BE, param, by=c("CODE_PAR"="CdParametre"))
    ref_I2M2_TBE<-ref_IBGN%>%subset(CODE_OPERATION%in%oper_a_garder_TBE & CODE_PAR%in%param$CdParametre)
    ref_I2M2_TBE<-left_join(ref_I2M2_TBE, param, by=c("CODE_PAR"="CdParametre"))
    
    data_graph<-dataIBG%>%subset(CODE_PAR%in%param$CdParametre)
    data_graph<-left_join(data_graph, param, by=c("CODE_PAR"="CdParametre"))
    data_graph$DATE<-as.Date(data_graph$DATE, format="%d/%m/%Y")
    lbl_dates<-unique(paste(data_graph$CODE_STATION, format(data_graph$DATE, "%d/%m/%y")))
    data_graph$lbl_date<-factor(paste(data_graph$CODE_STATION, format(data_graph$DATE, "%d/%m/%y")), 
                                labels=lbl_dates,
                                ordered=TRUE) 
    # graph IBG
    g_IBG<-ggplot(data_graph%>%subset(CODE_PAR=="5910"), aes(NomParametre, RESULTAT)) + 
      geom_violin(data=ref_I2M2_BE%>%subset(CODE_PAR=="5910"),aes(NomParametre, RESULTAT), fill="green")+ 
      geom_violin(data=ref_I2M2_TBE%>%subset(CODE_PAR=="5910"),aes(NomParametre, RESULTAT), fill="cyan")+ 
      geom_point()  + ylim(c(0,20)) + 
      theme(axis.text.x = element_text(angle = 0)) + xlab("") +
      facet_grid(lbl_date~NomParametre, scales = "free") + ylab("")
    # graph Var taxo
    
    g_vartax<-ggplot(data_graph%>%subset(CODE_PAR=="6034"), aes(NomParametre, RESULTAT)) + 
      geom_violin(data=ref_I2M2_BE%>%subset(CODE_PAR=="6034"),aes(NomParametre, RESULTAT), fill="green")+ 
      geom_violin(data=ref_I2M2_TBE%>%subset(CODE_PAR=="6034"),aes(NomParametre, RESULTAT), fill="cyan")+ 
      geom_point() + ylim(c(0,NA)) +
      theme(axis.text.x = element_text(angle = 0)) + xlab("") +
      facet_grid(lbl_date~NomParametre, scales = "free") + ylab("")
    
    # Graph GFI
    
    g_gfi<-ggplot(data_graph%>%subset(CODE_PAR=="6035"), aes(NomParametre, RESULTAT)) + 
      geom_violin(data=ref_I2M2_BE%>%subset(CODE_PAR=="6035"), aes(NomParametre, RESULTAT), fill="green")+ 
      geom_violin(data=ref_I2M2_TBE%>%subset(CODE_PAR=="6035"), aes(NomParametre, RESULTAT), fill="cyan")+ 
      geom_point() + ylim(c(0,9)) +
      theme(axis.text.x = element_text(angle = 0)) + xlab("") +
      facet_grid(lbl_date~NomParametre, scales = "free") + ylab("")
    
    
    plot_grid(
      g_IBG, g_vartax, g_gfi, nrow=1
    )
  
  }
  
  
  
# graph diag radar
  

  
# autres graphs  
  
    
}





# Définition de l'interface utilisateur (UI)
ui <- navbarPage(
  # Titre de la page
  title = "Valorisation données I2M2",
  
  ##### UI : Onglet "Charger fichier de données" #####
  tabPanel(
    "Charger fichier de données",
    p("Sélectionnez le fichier à sauvegarder (au format d'entrée du SEEE et avec 
      la typologie renseignée) puis cliquez sur Charger le fichier."),
    
    # Zone de chargement de fichier
    fileInput("file", "Sélectionnez un fichier texte (.txt)"),
    
    # Bouton de soumission
    actionButton("submit", "Charger le fichier"),
    
    # Zone de sortie pour afficher les informations sur le fichier chargé
    uiOutput("info"),
  
    # Zone de sortie pour afficher le contenu du fichier
    dataTableOutput("table")
  ),
  ###### UI :  Onglet "Visualisation des résultats" #####
    
  tabPanel(
    "Visualisation des résultats",
    sidebarLayout(
      
      # Sidebar panel for inputs 
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
  ##### UI : Onglet "Charger les références" #####
  tabPanel(
    "Charger les références",
    # Titre de la section
    h2("Charger les données des stations de référence"),
    
    # Texte d'information
    p(
      "Ce bouton permet de télécharger les données au niveau des stations de référence sur l'hydroécorégion massif armoricain avec l'API HUBEAU. "
    ),
    br(),
    p("ATTENTION LA MISE A JOUR DURE PLUSIEURS MINUTES."),
    
    # Bouton de téléchargement des données de référence
    actionButton("download_refs", "Charger les données")
  ),
  
  ##### Onglet "A propos" #####
  tabPanel(
    "A propos",
    # Texte d'information
    HTML(
      "Visualisation de données issues des opérations I2M2. Mai 2023. Application développée par Eaux & Vilaine. Contact : anthony.deburghrave [at] eaux-et-vilaine.bzh<br>
      L'application permet de comparer les résultats d'opérations I2M2 aux résultats obtenus sur le massif armoriccain pour les stations de références classées en bon ou très bon état.<br>
      <br>Les seuils retenu pour l'outil \"radar\" de diagnostic sont adaptés des propositions de <br>Aquabio / F. Labat. <a href='https://www.researchgate.net/publication/350895873_Proposition_de_nouvelles_valeurs_guides_provisoires_et_niveaux_de_confiance_associes_pour_l%27interpretation_de_l%27outil_diagnostique_invertebres'>Proposition de nouvelles valeurs guides provisoires et niveaux de confiance associés <br>pour l’interprétation de l’outil diagnostique invertébrés. 2021</a>"
    )
  )
)

##### Définition du serveur (Server) #####
server <- function(input, output, session) {

##### SERVER : chargement du fichier avec les opérations I2M2 #####
  
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
    
    # mise à jour des codes opération dans le sélecteur de l'onglet visualisation
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
    output$info <- renderUI({
      HTML(paste0(
        "Fichier chargé :",
        input$file$name,
        "<br>",
        "Nombre de lignes : ",
        nrow(content),
        "<br>",
        "Nombre de colonnes : ",
        ncol(content),
        "<br>"
      ))
    })
    
    # Affichage du contenu du fichier sous forme de tableau
    output$table <- renderDataTable({
      datatable(content ,
      filter="top")
    }, options = list(pageLength = 10))
  })
  
#####Server : onglet graph  #####
  
  # variable réactive pour enregistrer les résultats SEEE des opérations sélectionnées
  values_operations<-reactiveValues(OD=NULL,
                                    I2M2=NULL,
                                    IBG=NULL)
  
  
  # quand le bouton submit_operations est pressé, on appelle le SEEE pour calculer les métriques des opérations sélectionnées 
  observeEvent(input$submit_operations,{
    show_modal_spinner(text="Interrogation du SEEE / outil diagnostic")
    values_operations$OD<-calcule_SEEE_ODinvertebres(file_content%>%
                                                       subset(CODE_OPERATION%in%input$operation))
    show_modal_spinner(text="Interrogation du SEEE / outil I2M2")
    operations<<-input$operation
    values_operations$I2M2<-calcule_SEEE_I2M2(file_content%>%
                                                subset(CODE_OPERATION%in%input$operation)) 
    show_modal_spinner(text="Interrogation du SEEE / outil IBG-DCE")
    values_operations$IBG<-calcule_SEEE_IBG_DCE(file_content%>%
                                                  subset(CODE_OPERATION%in%input$operation)) 
    remove_modal_spinner()
      })
  
  ##### Graph #####
  output$graph_invertebre<-renderPlot(
    {
      dataI2M2<<-isolate(values_operations$I2M2)
      dataIBG<<-isolate(values_operations$IBG)
      dataOD<<-isolate(values_operations$OD)
      
      graphInv(type=input$type_indicateur,
               dataI2M2=values_operations$I2M2, 
               dataIBGN=values_operations$IBG, 
               dataOD=values_operations$OD,
               BE=ifelse("BON ETAT"%in%input$TBE_BE, TRUE, FALSE),
               TBE=ifelse("TRES BON ETAT"%in%input$TBE_BE, TRUE, FALSE))}
    
  )
  
##### Server : Chargement des données aux stations de référence #####    
  # Réaction au clic sur le bouton "download_refs"
  observeEvent(input$download_refs, {
    show_modal_progress_line() #indicateur de chargement
    # Charger les données indices invertébrés depuis hubeau pour totues les stations de référence
    stations_op <-
      tools4DCE::import_hubeau_indices_hbio(liste_stations = ref_staq_fr$Code.SANDRE.de.la.station,
                                            indice = "inv")  # mise en mémoire dans l'environnement global
    
    # on ne conserve que les opérations I2M2
    stations_op <- stations_op %>% subset(CdParametre == "7613")
    
    # pour chacune des stations de référence, on charge les listes faunistiques et on exécute le script SEEE / outil diag
    for (i in 1:length(ref_staq_fr$Code.SANDRE.de.la.station))
    {
      print(i)
      update_modal_progress((i - 0.8) / length(ref_staq_fr$Code.SANDRE.de.la.station)) # update progress bar value
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
      donnees$TYPO_NATIONALE <- ref_staq_fr[ref_staq_fr$Code.SANDRE.de.la.station==donnees$code_station_hydrobio,]$TYPEFR_CONSOLIDE[1]
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
      
      update_modal_progress((i - 0.5) / length(ref_staq_fr$Code.SANDRE.de.la.station)) # update progress bar value
      # Appel de l'outil diag du SEEE
      resultats_OD <- calcule_SEEE_ODinvertebres(donnees)
      update_modal_progress((i - 0.3) / length(ref_staq_fr$Code.SANDRE.de.la.station)) # update progress bar value 
      resultats_I2M2<-calcule_SEEE_I2M2(donnees)
      update_modal_progress(i / length(ref_staq_fr$Code.SANDRE.de.la.station)) # update progress bar value
      resultats_IBG<-calcule_SEEE_IBG_DCE(donnees)
      
      # agrégation des fichiers de résultat et des listes faunistiques
      ifelse(i == 1,
             ref_listes_fau <- donnees,
             ref_listes_fau <- rbind(donnees, ref_listes_fau))
      
      ifelse(i == 1,
             ref_OD <- resultats_OD,
             ref_OD <- rbind(resultats_OD, ref_OD))
      
      ifelse(i == 1,
             ref_I2M2 <- resultats_I2M2,
             ref_I2M2 <- rbind(resultats_I2M2, ref_I2M2))
      
      ifelse(i == 1,
             ref_IBGN <- resultats_IBG,
             ref_IBGN <- rbind(resultats_IBG, ref_IBGN))
    }
    
    # mise en mémoire dans l'environnement global
    ref_listes_fau <<- ref_listes_fau
    ref_OD <<- ref_OD
    ref_I2M2<<-ref_I2M2
    ref_IBGN<<-ref_IBGN
    saveRDS(ref_listes_fau, "data/listes_fau_stations_ref.rds")
    saveRDS(ref_OD, "data/od_stations_ref.rds")
    saveRDS(ref_I2M2, "data/i2m2_stations_ref.rds")
    saveRDS(ref_IBGN, "data/ibgn_stations_ref.rds")
    remove_modal_progress()
    
  })
  

  
  
}


# Lancement de l'application
shinyApp(ui = ui, server = server)
