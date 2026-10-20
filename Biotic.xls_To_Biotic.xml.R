# app.R
# 🚀 Chargement des packages
library(shiny)
library(bslib)       # Pour personnalisation des couleurs
library(readxl)
library(xml2)

# --- Interface utilisateur ---
ui <- fluidPage(
  # Thème personnalisé avec bslib : couleurs primaires et secondaires
  theme = bs_theme(
    version   = 4,
    bootswatch = "flatly",    # Base esthétique agréable
    primary    = "#1E6091",   # Bleu océan profond
    secondary  = "#2E86AB",   # Bleu marine clair
    success    = "#28a745",   # Vert
    danger     = "#dc3545"    # Rouge
  ),

  # Police Google + CSS personnalisé
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=Poppins:wght@600;700&display=swap"
    ),
    tags$link(
      rel  = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    ),
    tags$style(HTML("
      body {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif !important;
        background: linear-gradient(135deg, #eef3f8 0%, #d9e4f0 100%);
        min-height: 100vh;
      }
      h1, h2, h3, h4, h5 { font-family: 'Poppins', sans-serif !important; }

      /* --- En-tête --- */
      .app-header {
        background: linear-gradient(135deg, #1E6091 0%, #2E86AB 60%, #4BA3C7 100%);
        color: #ffffff;
        padding: 28px 30px;
        border-radius: 14px;
        box-shadow: 0 8px 24px rgba(30, 96, 145, 0.25);
        margin: 15px 0 25px 0;
        position: relative;
        overflow: hidden;
      }
      .app-header::after {
        content: '';
        position: absolute;
        right: -40px; top: -40px;
        width: 180px; height: 180px;
        background: rgba(255,255,255,0.08);
        border-radius: 50%;
      }
      .app-header h1 {
        margin: 0;
        font-size: 1.9rem;
        font-weight: 700;
        letter-spacing: 0.3px;
        color: #ffffff;
      }
      .app-header .subtitle {
        margin-top: 8px;
        color: rgba(255,255,255,0.88);
        font-size: 1rem;
        font-weight: 400;
      }

      /* --- Cartes --- */
      .card-custom {
        background: #ffffff;
        border-radius: 14px;
        box-shadow: 0 4px 16px rgba(0,0,0,0.06);
        padding: 22px;
        border: 1px solid rgba(30, 96, 145, 0.08);
      }
      .card-title {
        color: #1E6091;
        font-weight: 600;
        margin: 0 0 16px 0;
        padding-bottom: 10px;
        border-bottom: 2px solid #eef3f8;
        display: flex;
        align-items: center;
        gap: 10px;
        font-size: 1.1rem;
      }
      .card-title .fa, .card-title .fas, .card-title .far {
        color: #2E86AB;
      }

      /* --- Badge info --- */
      .info-badge {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        background: #e7f1fb;
        color: #1E6091;
        padding: 6px 12px;
        border-radius: 20px;
        font-size: 0.82rem;
        font-weight: 500;
        margin-bottom: 14px;
        border: 1px solid rgba(30, 96, 145, 0.15);
      }

      /* --- File input --- */
      .form-group label { font-weight: 600; color: #334155; }
      .btn-file {
        background: #1E6091 !important;
        border: none !important;
        color: #fff !important;
        border-radius: 6px !important;
      }
      .btn-file:hover { background: #174e75 !important; }

      /* --- Bouton de téléchargement --- */
      #downloadXML {
        background: linear-gradient(135deg, #1E6091 0%, #2E86AB 100%) !important;
        border: none !important;
        color: #ffffff !important;
        padding: 11px 18px !important;
        border-radius: 8px !important;
        font-weight: 600 !important;
        width: 100% !important;
        transition: all 0.25s ease !important;
        box-shadow: 0 3px 10px rgba(30, 96, 145, 0.25);
      }
      #downloadXML:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 16px rgba(30, 96, 145, 0.35);
        background: linear-gradient(135deg, #174e75 0%, #236d8c 100%) !important;
      }

      /* --- Étapes --- */
      .steps-box {
        background: #f8fafc;
        border-left: 3px solid #2E86AB;
        padding: 12px 14px;
        border-radius: 6px;
        margin-top: 6px;
      }
      .step {
        display: flex;
        align-items: center;
        gap: 10px;
        padding: 5px 0;
        color: #475569;
        font-size: 0.9rem;
      }
      .step-number {
        background: #1E6091;
        color: white;
        min-width: 22px;
        height: 22px;
        border-radius: 50%;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        font-size: 0.78rem;
        font-weight: 700;
      }

      /* --- Aperçu XML --- */
      .xml-preview-wrapper {
        background: #1e293b;
        border-radius: 10px;
        padding: 14px;
        max-height: 560px;
        overflow: auto;
        border: 1px solid #0f172a;
      }
      #xmlPreview {
        background: transparent !important;
        color: #e2e8f0 !important;
        border: none !important;
        font-family: 'JetBrains Mono', 'Consolas', 'Monaco', monospace !important;
        font-size: 0.85rem !important;
        white-space: pre-wrap;
        word-break: break-word;
        padding: 0 !important;
        margin: 0 !important;
      }

      /* --- Barre de séparation --- */
      hr {
        border: none;
        border-top: 1px dashed #cbd5e1;
        margin: 16px 0;
      }

      /* --- Pied de page --- */
      .app-footer {
        text-align: center;
        color: #64748b;
        padding: 18px;
        margin-top: 20px;
        font-size: 0.88rem;
      }
      .app-footer i { color: #1E6091; margin-right: 4px; }

      /* Sidebar/Main : enlever l'encadré par défaut */
      .well { background: transparent !important; border: none !important; box-shadow: none !important; padding: 0 !important; }
    "))
  ),

  # --- En-tête ---
  div(class = "container",
      div(class = "app-header",
          tags$h1(
            tags$i(class = "fas fa-exchange-alt", style = "margin-right:10px;"),
            "Convertisseur Biotic_xls_ To_ Biotic_XML"
          ),
          div(class = "subtitle",
              tags$i(class = "fas fa-info-circle"),
              " Transformation de fichiers Excel LPA vers le format XML NMDBiotic (StoX)"
          )
      )
  ),

  # --- Contenu principal ---
  div(class = "container",
      sidebarLayout(
        sidebarPanel(
          width = 4,
          div(class = "card-custom",
              div(class = "card-title",
                  tags$i(class = "fas fa-cog"),
                  span("Paramètres")
              ),

              div(class = "info-badge",
                  tags$i(class = "fas fa-lightbulb"),
                  span("Fichier .xlsx (4 feuilles attendues)")
              ),

              fileInput(
                inputId = "excelFile",
                label   = tagList(
                  tags$i(class = "fas fa-file-excel", style = "color:#217346; margin-right:6px;"),
                  "Import Biotic_LPA"
                ),
                accept      = c(".xlsx"),
                buttonLabel = "Browse…",
                placeholder = "Aucun fichier"
              ),

              tags$hr(),

              downloadButton(
                outputId = "downloadXML",
                label    = tagList(
                  tags$i(class = "fas fa-download", style = "margin-right:6px;"),
                  "Télécharger le XML"
                ),
                class    = "btn-primary"
              ),

              tags$hr(),

              div(class = "steps-box",
                  tags$div(style = "font-weight:600; color:#1E6091; margin-bottom:6px;",
                           tags$i(class = "fas fa-list-ol"), " Étapes"),
                  div(class = "step", span(class = "step-number", "1"), "Choisir le fichier Excel"),
                  div(class = "step", span(class = "step-number", "2"), "Attendre la génération du XML"),
                  div(class = "step", span(class = "step-number", "3"), "Télécharger le fichier Biotic_LPA.xml")
              )
          )
        ),

        mainPanel(
          width = 8,
          div(class = "card-custom",
              div(class = "card-title",
                  tags$i(class = "fas fa-code"),
                  span("Aperçu du XML généré")
              ),
              div(class = "xml-preview-wrapper",
                  verbatimTextOutput("xmlPreview")
              )
          )
        )
      )
  ),

  # --- Pied de page ---
  div(class = "container",
      div(class = "app-footer",
          tags$i(class = "fas fa-fish"),
          "NMDBiotic v3.1 • ",
          tags$i(class = "fas fa-code-branch"),
          "Convertisseur LPA"
      )
  )
)

# --- Logique serveur ---  (INCHANGÉE sauf le nom du fichier téléchargé)
server <- function(input, output, session) {

  # 1. Lecture des 4 feuilles avec barre de progression
  excelData <- reactive({
    req(input$excelFile)
    withProgress(message = "Lecture du fichier Excel...", value = 0.2, {
      missions      <- read_excel(input$excelFile$datapath, sheet = "missions", col_types = "text")
      fishstations  <- read_excel(input$excelFile$datapath, sheet = "fishstations", col_types = "text")
      catchesamples <- read_excel(input$excelFile$datapath, sheet = "catchesamples", col_types = "text")
      individuals   <- read_excel(input$excelFile$datapath, sheet = "individuals", col_types = "text")
      incProgress(0.2)
    })
    list(
      missions      = missions,
      fishstations  = fishstations,
      catchesamples = catchesamples,
      individuals   = individuals
    )
  })

  # 2. Construction de l'objet XML avec progression
  xmlDoc <- reactive({
    req(excelData())
    df <- excelData()
    withProgress(message = "Génération du XML...", value = 0, {
      incProgress(0.1, detail = "Création racine et mission")
      ns_uri <- "http://www.imr.no/formats/nmdbiotic/v3.1"
      doc <- xml_new_root("missions", ns = ns_uri)

      # <mission>
      m <- df$missions[1, ]
      missionNode <- xml_add_child(doc, "mission")
      xml_set_attrs(missionNode, c(
        missiontype   = m$missiontype,
        startyear     = m$startyear,
        platform      = m$platform,
        missionnumber = m$missionnumber
      ))
      for(col in c(
        "missiontypename","callsignal","platformname",
        "cruise","missionstartdate","missionstopdate","purpose"
      )) {
        xml_add_child(missionNode, col, m[[col]])
      }
      incProgress(0.2, detail = "Ajout des stations")

      # Stations de pêche
      n_fs <- nrow(df$fishstations)
      for(i in seq_len(n_fs)){
        incProgress(0.5/n_fs, detail = paste("Station", i, "sur", n_fs))
        fs <- df$fishstations[i, ]
        fsNode <- xml_add_child(missionNode, "fishstation")
        xml_set_attr(fsNode, "serialnumber", fs$serialnumber)
        for(col in setdiff(names(fs), "serialnumber")){
          val <- fs[[col]]
          if(!is.na(val) && nzchar(val)) xml_add_child(fsNode, col, val)
        }

        # Échantillons de capture
        cs_df <- subset(df$catchesamples, serialnumber == fs$serialnumber)
        for(j in seq_len(nrow(cs_df))) {
          cs <- cs_df[j, ]
          csNode <- xml_add_child(fsNode, "catchsample")
          xml_set_attr(csNode, "catchsampleid", cs$catchsampleid)
          for(col in setdiff(
            names(cs), c("catchsampleid","missionnumber","serialnumber")
          )) {
            val <- cs[[col]]
            if(!is.na(val) && nzchar(val)) xml_add_child(csNode, col, val)
          }

          # Individus
          ind_df <- subset(
            df$individuals,
            serialnumber  == fs$serialnumber &
              catchsampleid == cs$catchsampleid
          )
          for(k in seq_len(nrow(ind_df))) {
            ind <- ind_df[k, ]
            indNode <- xml_add_child(csNode, "individual")
            xml_set_attr(indNode, "specimenid", ind$specimenid)
            for(col in setdiff(
              names(ind), c("specimenid","missionnumber","serialnumber","catchsampleid")
            )) {
              val <- ind[[col]]
              if(!is.na(val) && nzchar(val)) xml_add_child(indNode, col, val)
            }
          }
        }
      }
      incProgress(1, detail = "Terminé !")
      doc
    })
  })

  # 3. Aperçu du XML
  output$xmlPreview <- renderText({
    req(xmlDoc())
    as.character(xmlDoc())
  })

  # 4. Téléchargement du XML  →  fichier nommé "Biotic_LPA.xml"
  output$downloadXML <- downloadHandler(
    filename = function() {
      "Biotic_LPA.xml"
    },
    content = function(file) {
      xml2::write_xml(xmlDoc(), file, options = "format")
    }
  )
}

# Lancer l'application
shinyApp(ui, server)
