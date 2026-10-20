# ============================================================================
# Excel XML Converter
# Application Shiny globale regroupant 4 applications indépendantes.
# Chaque application est intégrée telle quelle, sans modification de son code
# interne (UI / server / logique). L'intégration se fait par encapsulation :
#   - chaque script est sourcé dans un environnement isolé,
#   - la fonction shinyApp() y est neutralisée afin de capturer `ui` et `server`,
#   - les UI sont placées chacune dans un tabPanel d'un navbarPage global,
#   - les server sont invoqués côte à côte dans le server global.
# ============================================================================

library(shiny)
library(bslib)
library(shinyjs)

# --- Liste des applications à intégrer --------------------------------------
# Le nom de chaque entrée correspond exactement au nom de l'application
# et sera utilisé comme libellé du tabPanel.
app_files <- list(
  "BioData_To_Biotic.xls"        = "BioData_To_Biotic.xls.R",
  "Biotic.xls_To_Biotic.xml"     = "Biotic.xls_To_Biotic.xml.R",
  "NASC.xls_To_Acoustic.xml"     = "NASC.xls_To_Acoustic.xml.R",
  "Acoustic.xls_To_Acoustic.xml" = "Acoustic.xls_To_Acoustic.xml.R"
)

app_dir <- tryCatch(
  dirname(sys.frame(1)$ofile),
  error = function(e) getwd()
)
if (is.null(app_dir) || !nzchar(app_dir) || !dir.exists(app_dir)) {
  app_dir <- getwd()
}

# --- Chargement encapsulé de chaque application -----------------------------
load_app <- function(path) {
  env <- new.env(parent = globalenv())
  # Neutralisation des lanceurs pour capturer ui/server sans démarrer l'app.
  env$shinyApp <- function(ui = NULL, server = NULL, ...) {
    assign("ui",     ui,     envir = env)
    assign("server", server, envir = env)
    invisible(NULL)
  }
  env$runApp <- function(...) invisible(NULL)
  sys.source(path, envir = env, chdir = TRUE)
  list(ui = env$ui, server = env$server, env = env)
}

apps <- lapply(app_files, function(f) load_app(file.path(app_dir, f)))
names(apps) <- names(app_files)

# Panneau « Excel files model » (export des modèles data/)
source(file.path(app_dir, "excel_models_tab.R"), local = FALSE)

# --- Splash (écran d'accueil, inliné, sans fichier externe) -----------------
splash_ui <- tags$div(
  id = "app_splash",
  tags$style(HTML("
    #app_splash {
      position: fixed; inset: 0; z-index: 99999;
      display: flex; align-items: center; justify-content: center;
      background: linear-gradient(135deg, #0f2027 0%, #203a43 50%, #2c5364 100%);
      font-family: 'Inter', system-ui, -apple-system, 'Segoe UI', sans-serif;
      color: #f5f7fa;
      overflow: auto;
    }
    #app_splash .splash-card {
      max-width: 720px;
      width: min(92vw, 720px);
      padding: 48px 44px;
      background: rgba(255,255,255,0.06);
      border: 1px solid rgba(255,255,255,0.12);
      border-radius: 18px;
      backdrop-filter: blur(8px);
      box-shadow: 0 20px 60px rgba(0,0,0,0.35);
      text-align: center;
    }
    #app_splash .splash-title {
      font-size: 2.4rem; font-weight: 700; letter-spacing: 0.5px;
      margin: 0 0 8px 0;
    }
    #app_splash .splash-subtitle {
      font-size: 1rem; font-weight: 600; letter-spacing: 1.2px;
      color: #ffd166; margin-bottom: 28px;
      text-transform: uppercase;
    }
    #app_splash .splash-desc {
      font-size: 0.98rem; line-height: 1.6;
      color: rgba(245,247,250,0.88); margin-bottom: 28px;
    }
    #app_splash .splash-modules {
      display: grid; grid-template-columns: repeat(2, 1fr);
      gap: 10px; margin: 0 0 32px 0; text-align: left;
    }
    #app_splash .splash-modules div {
      background: rgba(255,255,255,0.05);
      border: 1px solid rgba(255,255,255,0.1);
      border-radius: 10px;
      padding: 10px 14px;
      font-size: 0.9rem;
    }
    #app_splash .splash-modules b { color: #ffd166; }
    #app_splash #btn_launch_app {
      background: linear-gradient(135deg, #ffd166 0%, #f4a261 100%);
      color: #1a1a1a; border: none; font-weight: 700;
      padding: 14px 36px; font-size: 1.05rem;
      border-radius: 999px; cursor: pointer;
      box-shadow: 0 8px 24px rgba(255,209,102,0.35);
      transition: transform .15s ease, box-shadow .15s ease;
    }
    #app_splash #btn_launch_app:hover {
      transform: translateY(-2px);
      box-shadow: 0 12px 30px rgba(255,209,102,0.5);
    }
    @media (max-width: 560px) {
      #app_splash .splash-modules { grid-template-columns: 1fr; }
      #app_splash .splash-title { font-size: 1.8rem; }
    }
  ")),
  tags$div(class = "splash-card",
    tags$h1(class = "splash-title", "Excel XML Converter"),
    tags$div(class = "splash-subtitle", "LPA – 2026 : by KMZ"),
    tags$p(class = "splash-desc",
      "Suite intégrée de conversion des données Excel vers les formats XML NMDBiotic et Acoustic Stox compatibles."),
    tags$div(class = "splash-modules",
      tags$div(tags$b("BioData"), " → Biotic.xls"),
      tags$div(tags$b("Biotic.xls"), " → Biotic.xml"),
      tags$div(tags$b("NASC"), " → Acoustic.xlsx"),
      tags$div(tags$b("Acoustic.xls"), " → Acoustic.xml")
    ),
    actionButton("btn_launch_app", "\U0001F680 Lancer l'application", class = "btn")
  )
)

# --- UI globale -------------------------------------------------------------
main_ui <- navbarPage(
  title = tagList(
    tags$img(
      src = "logo.PNG",
      height = "75",
      class = "me-2 align-text-top d-inline",
      style = "object-fit: contain; max-width: 240px; height: 75px; width: auto;",
      alt = ""
    ),
    "Excel XML Converter"
  ),
  id = "excel_xml_converter_main",
  theme = tryCatch(bslib::bs_theme(version = 5), error = function(e) NULL),
  header = tags$head(
    tags$style(HTML("
      /* Titre global (brand) */
      .navbar-brand { font-weight: 700; letter-spacing: 0.3px; }

      /* Conteneur nav : flex space-between entre titre/tabs et signature */
      .navbar > .container-fluid,
      .navbar > .container {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        justify-content: space-between;
      }
      .navbar .navbar-nav { align-items: center; }

      /* Signature laboratoire, alignée à droite (couleur = texte navbar pour thème clair/sombre) */
      .app-signature {
        margin-left: auto;
        padding: 6px 14px;
        color: var(--bs-navbar-color);
        font-family: 'Inter', system-ui, -apple-system, sans-serif;
        font-weight: 600;
        font-size: 0.88rem;
        letter-spacing: 0.4px;
        border-left: 1px solid rgba(0,0,0,0.14);
        border-left-color: color-mix(in srgb, var(--bs-navbar-color) 28%, transparent);
        display: inline-flex;
        align-items: center;
        white-space: nowrap;
        opacity: 0.95;
        flex-shrink: 0;
      }
      .app-signature::before {
        content: '';
        display: inline-block;
        width: 6px; height: 6px;
        border-radius: 50%;
        background: #ffd166;
        margin-right: 8px;
        box-shadow: 0 0 6px rgba(255,209,102,0.6);
      }

      .tab-content > .tab-pane { padding-top: 8px; }
    ")),
    tags$script(HTML("
      (function() {
        var SIG = 'LPA \\u2013 2026 : by KMZ';
        /* Uniquement la navbar du header global (exclut les barres des sous-applications dans les onglets). */
        function getGlobalHeaderNav() {
          var navs = document.querySelectorAll('nav.navbar');
          for (var i = 0; i < navs.length; i++) {
            var n = navs[i];
            if (!n.closest('.tab-content')) return n;
          }
          return null;
        }
        function stripNestedSignatures() {
          document.querySelectorAll('.tab-content .app-signature').forEach(function(el) {
            el.remove();
          });
        }
        function inject() {
          stripNestedSignatures();
          var nav = getGlobalHeaderNav();
          if (!nav) return false;
          if (nav.querySelector('.app-signature')) return true;
          var container = nav.querySelector('.container-fluid, .container') || nav;
          var span = document.createElement('span');
          span.className = 'app-signature';
          span.textContent = SIG;
          container.appendChild(span);
          return true;
        }
        if (document.readyState !== 'loading') inject();
        else document.addEventListener('DOMContentLoaded', inject);
        var tries = 0;
        var iv = setInterval(function() {
          tries++;
          if (inject() || tries > 20) clearInterval(iv);
        }, 200);
        var mo = new MutationObserver(function() { inject(); });
        mo.observe(document.body, { childList: true, subtree: true });
      })();
    "))
  ),
  tabPanel("BioData_To_Biotic.xls",        apps[["BioData_To_Biotic.xls"]]$ui),
  tabPanel("Biotic.xls_To_Biotic.xml",     apps[["Biotic.xls_To_Biotic.xml"]]$ui),
  tabPanel("NASC.xls_To_Acoustic.xml",     apps[["NASC.xls_To_Acoustic.xml"]]$ui),
  tabPanel("Acoustic.xls_To_Acoustic.xml", apps[["Acoustic.xls_To_Acoustic.xml"]]$ui),
  tabPanel("Excel files model", ui_excel_files_model()),
  tabPanel("User_Guide",
    fluidPage(
      br(),
      downloadButton("dl_user_guide", "User_Guide", class = "btn-primary", icon = icon("file-pdf"))
    )
  )
)

# Composition finale : splash superposé + navbarPage (masqué tant que splash visible)
ui <- tagList(
  useShinyjs(),
  tags$head(tags$style(HTML("#app_main_wrapper.splash-active { display: none !important; }"))),
  splash_ui,
  tags$div(id = "app_main_wrapper", class = "splash-active", main_ui)
)

# --- Server global : délègue à chaque server d'application ------------------
server <- function(input, output, session) {
  observeEvent(input$btn_launch_app, {
    shinyjs::hide("app_splash", anim = TRUE, animType = "fade", time = 0.35)
    shinyjs::removeClass(id = "app_main_wrapper", class = "splash-active")
  }, ignoreInit = TRUE)

  for (nm in names(apps)) {
    srv <- apps[[nm]]$server
    if (is.function(srv)) {
      try(srv(input, output, session), silent = FALSE)
    }
  }
  excel_models_server(input, output, session, file.path(app_dir, "data"))

  output$dl_user_guide <- downloadHandler(
    filename = function() "User_Guide.pdf",
    content  = function(file) {
      src <- file.path(app_dir, "docs", "User_Guide.pdf")
      file.copy(src, file, overwrite = TRUE)
    },
    contentType = "application/pdf"
  )
}

shinyApp(ui = ui, server = server)
