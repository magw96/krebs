#' Cancer-specific compartments for the encounter form.
#'
#' When the user picks an OncoTree value, we infer a "category"
#' (breast, colorectal, NSCLC, prostate, ...) and reveal only the
#' biomarker / staging fields that make clinical sense for that
#' neoplasm. All extra inputs are merged into the existing
#' `biomarkers` JSONB column at submit time — no schema change.
#'
#' Adding a new category:
#'   1. extend `cancer_category()` with a regex match.
#'   2. add a `<category>` block to `cancer_specific_ui()`.
#'   3. add a `<category>` block to `cancer_specific_values()`.

# ---------------------------------------------------------------------------
# 1. Inference
# ---------------------------------------------------------------------------

#' Map an OncoTree string to an internal category id.
#' @param oncotree character(1) — e.g. "Invasive Breast Carcinoma (BRCA)"
#' @return character(1) | NA
cancer_category <- function(oncotree) {
  if (is.null(oncotree) || !nzchar(oncotree)) return(NA_character_)
  s <- tolower(oncotree)
  if (grepl("breast",                          s)) return("breast")
  if (grepl("colon|rectal|colorectal|crc",     s)) return("colorectal")
  if (grepl("small cell lung|sclc",            s)) return("lung_sclc")
  if (grepl("non-small cell|nsclc|lung adeno|lung squamous|lung carcinoma", s))
                                                   return("lung_nsclc")
  if (grepl("prostate",                        s)) return("prostate")
  if (grepl("cervix|cervical",                 s)) return("gyn_cervical")
  if (grepl("ovari|fallop|peritoneal",         s)) return("gyn_ovarian")
  if (grepl("endometri|uterine",               s)) return("gyn_endometrial")
  if (grepl("thyroid",                         s)) return("thyroid")
  if (grepl("melanoma|cutaneous",              s)) return("melanoma")
  if (grepl("lymphoma|hodgkin",                s)) return("lymphoma")
  if (grepl("sarcoma|gist",                    s)) return("sarcoma")
  if (grepl("gastric|stomach",                 s)) return("gastric")
  if (grepl("hepatocellular|hcc|liver",        s)) return("hcc")
  if (grepl("pancrea",                         s)) return("pancreas")
  if (grepl("head and neck|oropharyn|larynx|nasophar|hypophar|oral cavity",
            s))                                    return("head_neck")
  if (grepl("renal|kidney",                    s)) return("renal")
  if (grepl("bladder|urothelial",              s)) return("bladder")
  if (grepl("glioma|glioblas|astrocyt|cns",    s)) return("cns_glioma")
  if (grepl("leukem|aml|all|cll|cml",          s)) return("leukemia")
  if (grepl("myeloma|plasma cell",             s)) return("myeloma")
  NA_character_
}

#' Pretty-print the category for the UI header.
cancer_category_label <- function(cat) {
  switch(cat %||% "",
    breast          = "Cáncer de mama",
    colorectal      = "Cáncer colorrectal",
    lung_nsclc      = "Cáncer de pulmón (NSCLC)",
    lung_sclc       = "Cáncer de pulmón (SCLC)",
    prostate        = "Cáncer de próstata",
    gyn_cervical    = "Cáncer cervicouterino",
    gyn_ovarian     = "Cáncer de ovario",
    gyn_endometrial = "Cáncer de endometrio",
    thyroid         = "Cáncer de tiroides",
    melanoma        = "Melanoma",
    lymphoma        = "Linfoma",
    sarcoma         = "Sarcoma / GIST",
    gastric         = "Cáncer gástrico",
    hcc             = "Hepatocarcinoma",
    pancreas        = "Cáncer de páncreas",
    head_neck       = "Cabeza y cuello",
    renal           = "Cáncer renal",
    bladder         = "Cáncer de vejiga",
    cns_glioma      = "Tumor del SNC",
    leukemia        = "Leucemia",
    myeloma         = "Mieloma",
    "")
}

# ---------------------------------------------------------------------------
# 2. UI per category
# ---------------------------------------------------------------------------

#' Build the cancer-specific UI block.
#' @param ns shiny namespace function from the parent module
#' @param category character(1) — output of cancer_category()
cancer_specific_ui <- function(ns, category) {
  if (is.na(category) || !nzchar(category)) return(NULL)
  body <- switch(category,
    breast = shiny::tagList(
      shiny::fluidRow(
        shiny::column(3, shinyWidgets::pickerInput(ns("cs_er"),  "ER",
          choices = c("","positivo","negativo","desconocido"))),
        shiny::column(3, shinyWidgets::pickerInput(ns("cs_pr"),  "PR",
          choices = c("","positivo","negativo","desconocido"))),
        shiny::column(3, shinyWidgets::pickerInput(ns("cs_her2"),"HER2",
          choices = c("","0","1+","2+ ISH-","2+ ISH+","3+","desconocido"))),
        shiny::column(3, shiny::numericInput(ns("cs_ki67"), "Ki-67 (%)",
          value = NA, min = 0, max = 100))
      ),
      shiny::fluidRow(
        shiny::column(6, shinyWidgets::pickerInput(ns("cs_subtype"),
          "Subtipo IHQ",
          choices = c("","Luminal A","Luminal B","HER2-enriquecido",
                      "Triple negativo"))),
        shiny::column(6, shinyWidgets::pickerInput(ns("cs_brca"),
          "BRCA germinal",
          choices = c("","BRCA1","BRCA2","Negativo","No realizado")))
      )
    ),

    colorectal = shiny::tagList(
      shiny::fluidRow(
        shiny::column(3, shinyWidgets::pickerInput(ns("cs_msi"),
          "MMR / MSI",
          choices = c("","pMMR / MSS","dMMR / MSI-H","desconocido"))),
        shiny::column(3, shinyWidgets::pickerInput(ns("cs_kras"),  "KRAS",
          choices = c("","wildtype","mutado","desconocido"))),
        shiny::column(3, shinyWidgets::pickerInput(ns("cs_nras"),  "NRAS",
          choices = c("","wildtype","mutado","desconocido"))),
        shiny::column(3, shinyWidgets::pickerInput(ns("cs_braf"),  "BRAF",
          choices = c("","wildtype","V600E","otro mutante","desconocido")))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("cs_cea"), "CEA basal (ng/mL)",
          value = NA, min = 0, step = 0.1)),
        shiny::column(6, shinyWidgets::pickerInput(ns("cs_site_cr"),
          "Lateralidad",
          choices = c("","Colon derecho","Colon izquierdo","Recto")))
      )
    ),

    lung_nsclc = shiny::tagList(
      shiny::fluidRow(
        shiny::column(3, shinyWidgets::pickerInput(ns("cs_egfr"), "EGFR",
          choices = c("","wildtype","Ex19del","L858R","T790M","otro","desconocido"))),
        shiny::column(3, shinyWidgets::pickerInput(ns("cs_alk"),  "ALK",
          choices = c("","positivo","negativo","desconocido"))),
        shiny::column(3, shinyWidgets::pickerInput(ns("cs_ros1"), "ROS1",
          choices = c("","positivo","negativo","desconocido"))),
        shiny::column(3, shiny::numericInput(ns("cs_pdl1"), "PD-L1 TPS (%)",
          value = NA, min = 0, max = 100))
      ),
      shiny::fluidRow(
        shiny::column(4, shinyWidgets::pickerInput(ns("cs_kras_lung"), "KRAS",
          choices = c("","wildtype","G12C","otro mutante","desconocido"))),
        shiny::column(4, shinyWidgets::pickerInput(ns("cs_smoking"),
          "Tabaquismo",
          choices = c("","Nunca","Ex-fumador","Activo"))),
        shiny::column(4, shiny::numericInput(ns("cs_pack_years"),
          "Paquetes-año", value = NA, min = 0))
      )
    ),

    lung_sclc = shiny::fluidRow(
      shiny::column(6, shinyWidgets::pickerInput(ns("cs_sclc_stage"),
        "Etapa (VALG)",
        choices = c("","Limitada","Extendida"))),
      shiny::column(6, shiny::numericInput(ns("cs_pack_years"),
        "Paquetes-año", value = NA, min = 0))
    ),

    prostate = shiny::tagList(
      shiny::fluidRow(
        shiny::column(3, shiny::numericInput(ns("cs_psa"),
          "PSA (ng/mL)", value = NA, min = 0, step = 0.01)),
        shiny::column(3, shiny::numericInput(ns("cs_gleason_a"),
          "Gleason 1°", value = NA, min = 1, max = 5)),
        shiny::column(3, shiny::numericInput(ns("cs_gleason_b"),
          "Gleason 2°", value = NA, min = 1, max = 5)),
        shiny::column(3, shinyWidgets::pickerInput(ns("cs_isup"),
          "ISUP grupo",
          choices = c("","1","2","3","4","5")))
      ),
      shiny::fluidRow(
        shiny::column(6, shinyWidgets::pickerInput(ns("cs_dam"),
          "Riesgo D'Amico",
          choices = c("","Bajo","Intermedio","Alto"))),
        shiny::column(6, shinyWidgets::awesomeCheckbox(ns("cs_castrate"),
          "Resistente a castración (CRPC)", FALSE))
      )
    ),

    gyn_cervical = shiny::fluidRow(
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_figo"),
        "FIGO 2018",
        choices = c("","IA1","IA2","IB1","IB2","IB3","IIA1","IIA2",
                    "IIB","IIIA","IIIB","IIIC1","IIIC2","IVA","IVB"))),
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_hpv_geno"),
        "Genotipo VPH",
        choices = c("","16","18","Otro alto riesgo","Bajo riesgo","Negativo"))),
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_p16"),
        "p16 IHQ",
        choices = c("","positivo","negativo","desconocido")))
    ),

    gyn_ovarian = shiny::tagList(
      shiny::fluidRow(
        shiny::column(4, shinyWidgets::pickerInput(ns("cs_figo_ov"),
          "FIGO ovario",
          choices = c("","IA","IB","IC","IIA","IIB","IIIA","IIIB","IIIC","IV"))),
        shiny::column(4, shiny::numericInput(ns("cs_ca125"),
          "CA-125 (U/mL)", value = NA, min = 0)),
        shiny::column(4, shinyWidgets::pickerInput(ns("cs_brca_ov"),
          "BRCA / HRD",
          choices = c("","BRCA1","BRCA2","HRD positivo","HRD negativo",
                      "No realizado")))
      )
    ),

    gyn_endometrial = shiny::fluidRow(
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_figo_em"),
        "FIGO endometrio",
        choices = c("","IA","IB","II","IIIA","IIIB","IIIC1","IIIC2","IVA","IVB"))),
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_em_mol"),
        "Subtipo molecular",
        choices = c("","POLE-mutado","MSI-H","p53-abn","NSMP / sin perfil"))),
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_em_histo"),
        "Histología",
        choices = c("","Endometrioide","Seroso","Células claras","Carcinosarcoma")))
    ),

    thyroid = shiny::fluidRow(
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_thy_histo"),
        "Histología",
        choices = c("","Papilar","Folicular","Medular","Anaplásico"))),
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_braf_thy"), "BRAF V600E",
        choices = c("","positivo","negativo","desconocido"))),
      shiny::column(4, shiny::numericInput(ns("cs_tg"),
        "Tiroglobulina (ng/mL)", value = NA, min = 0))
    ),

    melanoma = shiny::fluidRow(
      shiny::column(3, shiny::numericInput(ns("cs_breslow"),
        "Breslow (mm)", value = NA, min = 0, step = 0.1)),
      shiny::column(3, shinyWidgets::pickerInput(ns("cs_clark"),
        "Nivel de Clark",
        choices = c("","I","II","III","IV","V"))),
      shiny::column(3, shinyWidgets::awesomeCheckbox(ns("cs_ulcer"),
        "Ulceración", FALSE)),
      shiny::column(3, shinyWidgets::pickerInput(ns("cs_braf_mel"),
        "BRAF",
        choices = c("","V600E","V600K","wildtype","desconocido")))
    ),

    lymphoma = shiny::tagList(
      shiny::fluidRow(
        shiny::column(4, shinyWidgets::pickerInput(ns("cs_ann_arbor"),
          "Ann Arbor",
          choices = c("","I","II","III","IV"))),
        shiny::column(4, shinyWidgets::pickerInput(ns("cs_b_symp"),
          "Síntomas B",
          choices = c("","Sí","No"))),
        shiny::column(4, shinyWidgets::pickerInput(ns("cs_ipi"),
          "IPI / FLIPI",
          choices = c("","Bajo","Intermedio-bajo","Intermedio-alto","Alto")))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("cs_ldh"),
          "LDH (U/L)", value = NA, min = 0)),
        shiny::column(6, shinyWidgets::pickerInput(ns("cs_cell_origin"),
          "Origen celular",
          choices = c("","B-cell","T-cell","NK","Hodgkin clásico",
                      "Hodgkin nodular")))
      )
    ),

    sarcoma = shiny::fluidRow(
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_sar_grade"),
        "FNCLCC",
        choices = c("","G1","G2","G3"))),
      shiny::column(4, shiny::numericInput(ns("cs_size_cm"),
        "Tamaño tumoral (cm)", value = NA, min = 0, step = 0.1)),
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_kit"), "KIT / PDGFRA",
        choices = c("","KIT exón 11","KIT exón 9","PDGFRA","wildtype",
                    "no aplica")))
    ),

    gastric = shiny::fluidRow(
      shiny::column(3, shinyWidgets::pickerInput(ns("cs_lauren"),
        "Lauren",
        choices = c("","Intestinal","Difuso","Mixto"))),
      shiny::column(3, shinyWidgets::pickerInput(ns("cs_her2_ga"), "HER2",
        choices = c("","0","1+","2+ ISH-","2+ ISH+","3+"))),
      shiny::column(3, shinyWidgets::pickerInput(ns("cs_msi_ga"), "MSI",
        choices = c("","MSS","MSI-H","desconocido"))),
      shiny::column(3, shinyWidgets::pickerInput(ns("cs_pdl1_ga"),
        "PD-L1 CPS",
        choices = c("","<1","1-4","5-9","≥10")))
    ),

    hcc = shiny::fluidRow(
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_bclc"),
        "BCLC",
        choices = c("","0","A","B","C","D"))),
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_child"),
        "Child-Pugh",
        choices = c("","A","B","C"))),
      shiny::column(4, shiny::numericInput(ns("cs_afp"),
        "AFP (ng/mL)", value = NA, min = 0))
    ),

    pancreas = shiny::fluidRow(
      shiny::column(4, shiny::numericInput(ns("cs_ca199"),
        "CA 19-9 (U/mL)", value = NA, min = 0)),
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_resect"),
        "Resecabilidad",
        choices = c("","Resecable","Limítrofe","Localmente avanzado",
                    "Metastásico"))),
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_brca_panc"),
        "BRCA / HRD",
        choices = c("","BRCA1","BRCA2","HRD positivo","Negativo",
                    "No realizado")))
    ),

    head_neck = shiny::fluidRow(
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_p16_hn"),
        "p16 / VPH",
        choices = c("","p16+ VPH+","p16- VPH-","desconocido"))),
      shiny::column(4, shiny::numericInput(ns("cs_pdl1_hn"),
        "PD-L1 CPS", value = NA, min = 0, max = 100)),
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_smoking_hn"),
        "Tabaco / alcohol",
        choices = c("","Ninguno","Solo tabaco","Solo alcohol","Ambos")))
    ),

    renal = shiny::fluidRow(
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_rcc_histo"),
        "Histología",
        choices = c("","Células claras","Papilar","Cromófobo",
                    "Sarcomatoide","Otro"))),
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_imdc"),
        "IMDC",
        choices = c("","Favorable","Intermedio","Pobre"))),
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_furhman"),
        "Fuhrman",
        choices = c("","1","2","3","4")))
    ),

    bladder = shiny::fluidRow(
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_invasive"),
        "Invasión muscular",
        choices = c("","NMIBC","MIBC","desconocido"))),
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_grade_bl"),
        "Grado OMS",
        choices = c("","Bajo grado","Alto grado"))),
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_pdl1_bl"),
        "PD-L1",
        choices = c("","positivo","negativo","desconocido")))
    ),

    cns_glioma = shiny::fluidRow(
      shiny::column(3, shinyWidgets::pickerInput(ns("cs_who_cns"),
        "Grado OMS",
        choices = c("","1","2","3","4"))),
      shiny::column(3, shinyWidgets::pickerInput(ns("cs_idh"), "IDH",
        choices = c("","mutado","wildtype","desconocido"))),
      shiny::column(3, shinyWidgets::pickerInput(ns("cs_1p19q"), "1p/19q",
        choices = c("","Co-deleción","Intacto","desconocido"))),
      shiny::column(3, shinyWidgets::pickerInput(ns("cs_mgmt"), "MGMT",
        choices = c("","Metilado","No metilado","desconocido")))
    ),

    leukemia = shiny::fluidRow(
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_leuk_type"),
        "Tipo",
        choices = c("","AML","ALL","CML","CLL","Otra"))),
      shiny::column(4, shiny::numericInput(ns("cs_blasts"),
        "Blastos en MO (%)", value = NA, min = 0, max = 100)),
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_cytogen"),
        "Citogenética",
        choices = c("","Favorable","Intermedio","Adverso","Pendiente")))
    ),

    myeloma = shiny::fluidRow(
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_iss"),
        "ISS / R-ISS",
        choices = c("","I","II","III"))),
      shiny::column(4, shiny::numericInput(ns("cs_mpc"),
        "Plasmocitos MO (%)", value = NA, min = 0, max = 100)),
      shiny::column(4, shinyWidgets::pickerInput(ns("cs_chain"),
        "Cadena ligera",
        choices = c("","Kappa","Lambda","No secretor")))
    ),
    NULL
  )
  if (is.null(body)) return(NULL)

  bs4Dash::box(
    title = shiny::tagList(shiny::icon("ribbon"),
      sprintf(" Datos específicos — %s", cancer_category_label(category))),
    width = 12, collapsible = TRUE, status = "danger", solidHeader = TRUE,
    shiny::div(class = "cancer-specific-card p-2", body)
  )
}

# ---------------------------------------------------------------------------
# 3. Pull values back out at submit
# ---------------------------------------------------------------------------

#' Collect cancer-specific input values into a named list.
#' Skips NA / "" entries so the JSONB stays compact.
cancer_specific_values <- function(input, category) {
  if (is.na(category) || !nzchar(category)) return(list())
  keys <- switch(category,
    breast          = c("cs_er","cs_pr","cs_her2","cs_ki67","cs_subtype","cs_brca"),
    colorectal      = c("cs_msi","cs_kras","cs_nras","cs_braf","cs_cea","cs_site_cr"),
    lung_nsclc      = c("cs_egfr","cs_alk","cs_ros1","cs_pdl1","cs_kras_lung",
                        "cs_smoking","cs_pack_years"),
    lung_sclc       = c("cs_sclc_stage","cs_pack_years"),
    prostate        = c("cs_psa","cs_gleason_a","cs_gleason_b","cs_isup",
                        "cs_dam","cs_castrate"),
    gyn_cervical    = c("cs_figo","cs_hpv_geno","cs_p16"),
    gyn_ovarian     = c("cs_figo_ov","cs_ca125","cs_brca_ov"),
    gyn_endometrial = c("cs_figo_em","cs_em_mol","cs_em_histo"),
    thyroid         = c("cs_thy_histo","cs_braf_thy","cs_tg"),
    melanoma        = c("cs_breslow","cs_clark","cs_ulcer","cs_braf_mel"),
    lymphoma        = c("cs_ann_arbor","cs_b_symp","cs_ipi","cs_ldh",
                        "cs_cell_origin"),
    sarcoma         = c("cs_sar_grade","cs_size_cm","cs_kit"),
    gastric         = c("cs_lauren","cs_her2_ga","cs_msi_ga","cs_pdl1_ga"),
    hcc             = c("cs_bclc","cs_child","cs_afp"),
    pancreas        = c("cs_ca199","cs_resect","cs_brca_panc"),
    head_neck       = c("cs_p16_hn","cs_pdl1_hn","cs_smoking_hn"),
    renal           = c("cs_rcc_histo","cs_imdc","cs_furhman"),
    bladder         = c("cs_invasive","cs_grade_bl","cs_pdl1_bl"),
    cns_glioma      = c("cs_who_cns","cs_idh","cs_1p19q","cs_mgmt"),
    leukemia        = c("cs_leuk_type","cs_blasts","cs_cytogen"),
    myeloma         = c("cs_iss","cs_mpc","cs_chain"),
    character(0)
  )
  out <- list()
  for (k in keys) {
    v <- input[[k]]
    if (is.null(v)) next
    if (is.logical(v)) { out[[k]] <- isTRUE(v); next }
    if (is.numeric(v) && !is.na(v)) { out[[k]] <- v; next }
    if (is.character(v) && nzchar(v)) { out[[k]] <- v; next }
  }
  out$cancer_category <- category
  out
}
