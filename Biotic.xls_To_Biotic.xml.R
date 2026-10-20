# app.R - Biotic.xlsx -> NMDBiotic v3.1 XML
# 🚀 Packages
library(shiny)
library(bslib)       # Theme colours
library(readxl)
library(xml2)
if (!exists("validate_biotic", mode = "function")) {
  for (h in file.path("helpers", c("code_lists.R", "taxonomy.R", "validation.R"))) source(h, local = TRUE)
}

# --- NMDBiotic v3.1 element order ---
# Child elements are an xs:sequence in bioticv3_1.xsd, so they must be written
# in this order. Columns that are not NMDBiotic elements are left out.
BIOTIC_ELEMENT_ORDER <- list(
  mission = c(
    "missiontypename", "callsignal", "platformname", "cruise",
    "missionstartdate", "missionstopdate", "purpose"
  ),
  fishstation = c(
    "nation", "catchplatform", "station", "fixedstation", "stationstartdate",
    "stationstarttime", "stationstopdate", "stationstoptime", "stationtype",
    "latitudestart", "longitudestart", "latitudeend", "longitudeend", "system",
    "area", "location", "bottomdepthstart", "bottomdepthstop", "bottomdepthmean",
    "fishingdepthstart", "fishingdepthstop", "fishingdepthcount", "fishingdepthmax",
    "fishingdepthmin", "fishingdepthmean", "fishingdepthtemperature", "gearno",
    "sweeplength", "gear", "gearcount", "direction", "gearflow", "vesselspeed",
    "logstart", "logstop", "distance", "gearcondition", "samplequality",
    "verticaltrawlopening", "verticaltrawlopeningsd", "trawldoortype",
    "trawldoorarea", "trawldoorweight", "trawldoorspread", "trawldoorspreadsd",
    "wingspread", "wingspreadsd", "wirelength", "wirediameter", "wiredensity",
    "soaktime", "fishingbait", "tripno", "fishabundance", "fishdistribution",
    "landingsite", "fishingground", "vesselcount", "dataquality", "haulvalidity",
    "flora", "vegetationcover", "visibility", "waterlevel", "winddirection",
    "windspeed", "clouds", "sea", "weather", "stationcomment"
  ),
  catchsample = c(
    "commonname", "catchcategory", "catchpartnumber", "aphia", "scientificname",
    "identification", "foreignobject", "sampletype", "group", "conservation",
    "catchproducttype", "raisingfactor", "catchweight", "catchvolume", "catchcount",
    "abundancecategory", "sampleproducttype", "lengthmeasurement",
    "lengthsampleweight", "lengthsamplevolume", "lengthsamplecount",
    "specimensamplecount", "agesamplecount", "agingstructure", "parasite",
    "stomach", "intestine", "tissuesample", "samplerecipient", "catchcomment"
  ),
  individual = c(
    "individualproducttype", "individualweight", "individualvolume",
    "lengthresolution", "length", "fat", "fatpercent", "sex", "maturationstage",
    "specialstage", "eggstage", "moultingstage", "spawningfrequency",
    "stomachfillfield", "stomachfilllab", "digestion", "liver", "liverparasite",
    "gillworms", "swollengills", "fungusheart", "fungusspores", "fungusouter",
    "blackspot", "vertebraecount", "gonadweight", "liverweight", "stomachweight",
    "diameter", "mantlelength", "carapacelength", "headlength", "snouttoendoftail",
    "snouttoendsqueezed", "snouttoanalfin", "snouttodorsalfin", "forklength",
    "snouttoboneknob", "lengthwithouthead", "carapacewidth", "rightclawwidth",
    "rightclawlength", "rightclawheight", "meroswidth", "meroslength",
    "japanesecut", "abdomenwidth", "tissuesamplenumber", "morphologytype",
    "individualcomment", "preferredagereading"
  )
)
# Key columns written as attributes or used to link the levels
BIOTIC_KEY_COLUMNS <- c(
  "missiontype", "startyear", "platform", "missionnumber",
  "serialnumber", "catchsampleid", "specimenid"
)

# Adds the non-empty values of one row as child elements, in schema order
add_biotic_children <- function(node, row, level) {
  for (col in BIOTIC_ELEMENT_ORDER[[level]]) {
    if (!col %in% names(row)) next
    val <- row[[col]]
    if (!is.na(val) && nzchar(val) && !val %in% c("NA", "Inf", "-Inf", "NaN")) {
      xml_add_child(node, col, val)
    }
  }
}

# Columns of a sheet that have no NMDBiotic element at that level
biotic_unknown_columns <- function(df, level) {
  setdiff(names(df), c(BIOTIC_ELEMENT_ORDER[[level]], BIOTIC_KEY_COLUMNS))
}

# --- Interface utilisateur ---
ui <- fluidPage(
  # bslib theme: primary and secondary colours
  theme = bs_theme(
    version   = 4,
    bootswatch = "flatly",    # Base theme
    primary    = "#1E6091",   # Deep ocean blue
    secondary  = "#2E86AB",   # Light navy blue
    success    = "#28a745",   # Green
    danger     = "#dc3545"    # Red
  ),

  # Google fonts + custom CSS
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

      /* --- Header --- */
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

      /* --- Cards --- */
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

      /* --- Info badge --- */
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

      /* --- Download button --- */
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

      /* --- Steps --- */
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

      /* --- XML preview --- */
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

      /* --- Separator --- */
      hr {
        border: none;
        border-top: 1px dashed #cbd5e1;
        margin: 16px 0;
      }

      /* --- Footer --- */
      .app-footer {
        text-align: center;
        color: #64748b;
        padding: 18px;
        margin-top: 20px;
        font-size: 0.88rem;
      }
      .app-footer i { color: #1E6091; margin-right: 4px; }

      /* Sidebar/Main: remove the default frame */
      .well { background: transparent !important; border: none !important; box-shadow: none !important; padding: 0 !important; }
    "))
  ),

  # --- Header ---
  div(class = "container",
      div(class = "app-header",
          tags$h1(
            tags$i(class = "fas fa-exchange-alt", style = "margin-right:10px;"),
            "Biotic.xls → Biotic.xml converter"
          ),
          div(class = "subtitle",
              tags$i(class = "fas fa-info-circle"),
              " Converts a Biotic workbook to the NMDBiotic XML format (StoX)"
          )
      )
  ),

  # --- Main content ---
  div(class = "container",
      sidebarLayout(
        sidebarPanel(
          width = 4,
          div(class = "card-custom",
              div(class = "card-title",
                  tags$i(class = "fas fa-cog"),
                  span("Settings")
              ),

              div(class = "info-badge",
                  tags$i(class = "fas fa-lightbulb"),
                  span(".xlsx file (4 sheets expected)")
              ),

              fileInput(
                inputId = "excelFile",
                label   = tagList(
                  tags$i(class = "fas fa-file-excel", style = "color:#217346; margin-right:6px;"),
                  "Import Biotic workbook (.xlsx)"
                ),
                accept      = c(".xlsx"),
                buttonLabel = "Browse…",
                placeholder = "No file"
              ),
              fileInput(
                inputId = "xmlValidateFile",
                label   = tagList(
                  tags$i(class = "fas fa-clipboard-check", style = "margin-right:6px;"),
                  "…or validate an existing Biotic XML"
                ),
                accept      = c(".xml"),
                buttonLabel = "Browse…",
                placeholder = "No file"
              ),

              tags$hr(),

              downloadButton(
                outputId = "downloadXML",
                label    = tagList(
                  tags$i(class = "fas fa-download", style = "margin-right:6px;"),
                  "Download the XML"
                ),
                class    = "btn-primary"
              ),
              checkboxInput("overrideErrors", "Allow the download even if validation finds errors", FALSE),

              tags$hr(),

              div(class = "steps-box",
                  tags$div(style = "font-weight:600; color:#1E6091; margin-bottom:6px;",
                           tags$i(class = "fas fa-list-ol"), " Steps"),
                  div(class = "step", span(class = "step-number", "1"), "Choose the Excel file"),
                  div(class = "step", span(class = "step-number", "2"), "Wait for the XML to be generated"),
                  div(class = "step", span(class = "step-number", "3"), "Download Biotic.xml")
              )
          )
        ),

        mainPanel(
          width = 8,
          div(class = "card-custom mb-3",
              div(class = "card-title",
                  tags$i(class = "fas fa-clipboard-check"),
                  span("Validation")
              ),
              uiOutput("validationSummary"),
              DT::DTOutput("validationTable"),
              div(class = "mt-2 d-flex flex-wrap gap-2",
                  downloadButton("downloadFindings", "Download findings (CSV)", class = "btn-outline-secondary btn-sm"),
                  actionButton("checkWorms", "Check unresolved names in WoRMS", icon = icon("globe"),
                               class = "btn-outline-primary btn-sm")
              ),
              uiOutput("wormsCheckResult")
          ),
          div(class = "card-custom",
              div(class = "card-title",
                  tags$i(class = "fas fa-code"),
                  span("Generated XML preview")
              ),
              div(class = "xml-preview-wrapper",
                  verbatimTextOutput("xmlPreview")
              )
          )
        )
      )
  ),

  # --- Footer ---
  div(class = "container",
      div(class = "app-footer",
          tags$i(class = "fas fa-fish"),
          "NMDBiotic v3.1 • ",
          tags$i(class = "fas fa-code-branch"),
          "Nansen Converter"
      )
  )
)

# --- Server logic ---
server <- function(input, output, session) {

  # 1. Read the 4 sheets, with a progress bar
  excelData <- reactive({
    req(input$excelFile)
    withProgress(message = "Reading the Excel file...", value = 0.2, {
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

  # 2. Build the XML document, with progress
  xmlDoc <- reactive({
    req(excelData())
    df <- excelData()
    withProgress(message = "Generating the XML...", value = 0, {
      incProgress(0.1, detail = "Root and mission")
      ns_uri <- "http://www.imr.no/formats/nmdbiotic/v3.1"
      # xmlns declares the default namespace; a plain "ns" attribute would leave
      # the document without one and StoX would not recognise it as NMDBiotic.
      doc <- xml_new_root("missions", xmlns = ns_uri)

      # <mission>
      m <- df$missions[1, ]
      missionNode <- xml_add_child(doc, "mission")
      xml_set_attrs(missionNode, c(
        missiontype   = m$missiontype,
        startyear     = m$startyear,
        platform      = m$platform,
        missionnumber = m$missionnumber
      ))
      add_biotic_children(missionNode, m, "mission")
      incProgress(0.2, detail = "Adding stations")

      skipped <- c(
        biotic_unknown_columns(df$missions, "mission"),
        biotic_unknown_columns(df$fishstations, "fishstation"),
        biotic_unknown_columns(df$catchesamples, "catchsample"),
        biotic_unknown_columns(df$individuals, "individual")
      )
      if (length(skipped)) {
        showNotification(
          paste("Columns that are not NMDBiotic elements were left out of the XML:",
                paste(unique(skipped), collapse = ", ")),
          type = "warning", duration = 10
        )
      }
      missing_val <- function(x) is.na(x) | !nzchar(x) | x == "NA"
      n_no_aphia <- if ("aphia" %in% names(df$catchesamples)) {
        sum(missing_val(df$catchesamples$aphia))
      } else {
        nrow(df$catchesamples)
      }
      if (n_no_aphia > 0) {
        showNotification(
          paste0(n_no_aphia, " catch sample(s) have no aphia code, which NMDBiotic requires. ",
                 "Match their species to WoRMS in Catchsamples."),
          type = "warning", duration = 12
        )
      }
      n_no_specimen <- 0L

      # Fishing stations
      n_fs <- nrow(df$fishstations)
      for(i in seq_len(n_fs)){
        incProgress(0.5/n_fs, detail = paste("Station", i, "of", n_fs))
        fs <- df$fishstations[i, ]
        fsNode <- xml_add_child(missionNode, "fishstation")
        xml_set_attr(fsNode, "serialnumber", fs$serialnumber)
        add_biotic_children(fsNode, fs, "fishstation")

        # Catch samples
        cs_df <- subset(df$catchesamples, serialnumber == fs$serialnumber)
        for(j in seq_len(nrow(cs_df))) {
          cs <- cs_df[j, ]
          csNode <- xml_add_child(fsNode, "catchsample")
          xml_set_attr(csNode, "catchsampleid", cs$catchsampleid)
          add_biotic_children(csNode, cs, "catchsample")

          # Individuals
          ind_df <- subset(
            df$individuals,
            serialnumber  == fs$serialnumber &
              catchsampleid == cs$catchsampleid
          )
          for(k in seq_len(nrow(ind_df))) {
            ind <- ind_df[k, ]
            # specimenid is a required integer attribute
            if (missing_val(ind$specimenid)) {
              n_no_specimen <- n_no_specimen + 1L
              next
            }
            indNode <- xml_add_child(csNode, "individual")
            xml_set_attr(indNode, "specimenid", ind$specimenid)
            add_biotic_children(indNode, ind, "individual")
          }
        }
      }
      if (n_no_specimen > 0) {
        showNotification(
          paste0(n_no_specimen, " individual(s) without a specimenid were left out of the XML."),
          type = "warning", duration = 12
        )
      }
      incProgress(1, detail = "Done!")
      doc
    })
  })

  # 3. XML preview
  output$xmlPreview <- renderText({
    req(xmlDoc())
    as.character(xmlDoc())
  })

  # 4. Download the XML as "Biotic.xml"
  output$downloadXML <- downloadHandler(
    filename = function() {
      "Biotic.xml"
    },
    content = function(file) {
      xml2::write_xml(xmlDoc(), file, options = "format")
    }
  )

  # 5. Validation: data checks, NMD schema and read-back with StoX's reader
  cl <- code_lists_get(session)

  convertedValidation <- reactive({
    req(excelData(), xmlDoc())
    tabs <- lapply(excelData(), as.data.frame)
    withProgress(message = "Validating...", value = 0.3, {
      f_data <- validate_biotic(tabs, cl$lists)
      incProgress(0.4)
      tmp <- tempfile(fileext = ".xml")
      xml2::write_xml(xmlDoc(), tmp)
      f_schema <- validate_xml_schema(tmp, "biotic")
      counts <- xml_element_counts(xmlDoc(), c("fishstation", "catchsample", "individual"))
      f_read <- readback_xml(tmp, "biotic", counts)
      rbind(f_data, f_schema, f_read)
    })
  })

  uploadedValidation <- reactive({
    req(input$xmlValidateFile)
    p <- input$xmlValidateFile$datapath
    withProgress(message = "Validating the uploaded XML...", value = 0.3, {
      f_schema <- validate_xml_schema(p, "biotic")
      tabs <- tryCatch(biotic_tables_from_xml(p), error = function(e) e)
      if (inherits(tabs, "error")) {
        fc <- findings_collector()
        fc$add("error", "read-back", "READBACK_FAILED", "file", paste("StoX's reader could not read the file:", conditionMessage(tabs)))
        return(rbind(fc$get(), f_schema))
      }
      rbind(validate_biotic(tabs, cl$lists), f_schema)
    })
  })

  # The panel shows whichever was produced last: the converted file or an upload
  shownValidation <- reactiveVal(NULL)
  observeEvent(convertedValidation(), shownValidation(list(what = "the converted Biotic data",
                                                           f = convertedValidation())))
  observeEvent(uploadedValidation(), shownValidation(list(what = paste("the uploaded file", input$xmlValidateFile$name),
                                                          f = uploadedValidation())))

  output$validationSummary <- renderUI({
    v <- shownValidation()
    if (is.null(v)) return(p(class = "text-muted small", "Load a Biotic workbook, or an existing Biotic XML, to validate it."))
    tagList(p(class = "small mb-1", "Results for ", strong(v$what), ":"), findings_badges(v$f))
  })
  output$validationTable <- DT::renderDT({
    v <- shownValidation()
    req(v)
    findings_dt(v$f)
  })
  output$downloadFindings <- downloadHandler(
    filename = function() paste0("Biotic_validation_", Sys.Date(), ".csv"),
    content = function(file) {
      v <- shownValidation()
      utils::write.csv(if (is.null(v)) data.frame() else v$f, file, row.names = FALSE)
    }
  )

  # Errors in the converted data block the download unless overridden
  observe({
    f <- tryCatch(convertedValidation(), error = function(e) NULL)
    ok <- !is.null(f) && (sum(f$severity == "error") == 0 || isTRUE(input$overrideErrors))
    shinyjs::toggleState("downloadXML", condition = ok)
  })

  wormsCheck <- reactiveVal(NULL)
  observeEvent(input$checkWorms, {
    v <- shownValidation()
    nm <- if (is.null(v)) character(0) else v$f$where[v$f$check == "TAX_UNRESOLVED"]
    if (!length(nm)) {
      wormsCheck("No unresolved names to check.")
      return()
    }
    withProgress(message = "Checking names in WoRMS…", value = 0.4, {
      wormsCheck(taxonomy_check_worms(nm))
    })
  })
  output$wormsCheckResult <- renderUI({
    res <- wormsCheck()
    if (is.null(res)) return(NULL)
    if (is.character(res)) return(div(class = "alert alert-info small mt-2", res))
    tagList(h6(class = "mt-3", "WoRMS check"), tableOutput("wormsCheckTable"))
  })
  output$wormsCheckTable <- renderTable({
    res <- wormsCheck()
    req(is.data.frame(res))
    res[, intersect(c("query", "worms_status", "valid_name", "valid_AphiaID", "note"), names(res)), drop = FALSE]
  }, striped = TRUE, spacing = "xs")
}

# Launch the application
shinyApp(ui, server)
