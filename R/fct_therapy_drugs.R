#' Therapy drug catalog (NCCN / ESMO international references).
#'
#' Returns character vectors of generic drug names per cancer category and
#' therapy kind ("hormonal", "targeted", "immuno"). All strings are pure ASCII
#' so the file parses under any locale (PCC may run in C / POSIX).
#'
#' Usage:
#'   lookup_therapy_drugs("breast", "hormonal")
#'   lookup_therapy_drugs(cancer_cat(), "targeted")
#'
#' Falls back to a generic class-level list when the category is unknown or has
#' no specific entry, so the picker is never empty.

# ---------------------------------------------------------------------------
# Generic class-level fallbacks (used when category is unknown)
# ---------------------------------------------------------------------------

.therapy_generic <- list(
  hormonal = c(
    "Tamoxifeno", "Anastrozol", "Letrozol", "Exemestano",
    "Fulvestrant", "Leuprolide", "Goserelina", "Triptorelina",
    "Bicalutamida", "Enzalutamida", "Apalutamida", "Darolutamida",
    "Abiraterona", "Megestrol", "Medroxiprogesterona", "Octreotide",
    "Lanreotide"
  ),
  targeted = c(
    "Trastuzumab", "Pertuzumab", "T-DM1 (ado-trastuzumab emtansina)",
    "T-DXd (trastuzumab deruxtecan)", "Lapatinib", "Tucatinib", "Neratinib",
    "Imatinib", "Dasatinib", "Nilotinib", "Bosutinib", "Ponatinib",
    "Erlotinib", "Gefitinib", "Afatinib", "Osimertinib", "Dacomitinib",
    "Crizotinib", "Alectinib", "Brigatinib", "Lorlatinib", "Ceritinib",
    "Entrectinib", "Larotrectinib", "Selpercatinib", "Pralsetinib",
    "Sotorasib", "Adagrasib", "Capmatinib", "Tepotinib",
    "Bevacizumab", "Ramucirumab", "Aflibercept",
    "Cetuximab", "Panitumumab",
    "Olaparib", "Niraparib", "Rucaparib", "Talazoparib",
    "Palbociclib", "Ribociclib", "Abemaciclib",
    "Everolimus", "Temsirolimus", "Alpelisib",
    "Vemurafenib", "Dabrafenib", "Encorafenib", "Trametinib", "Cobimetinib",
    "Binimetinib", "Sorafenib", "Sunitinib", "Pazopanib", "Regorafenib",
    "Cabozantinib", "Lenvatinib", "Axitinib", "Vandetanib",
    "Ibrutinib", "Acalabrutinib", "Zanubrutinib",
    "Venetoclax", "Idelalisib", "Duvelisib",
    "Rituximab", "Obinutuzumab", "Ofatumumab", "Polatuzumab vedotina",
    "Brentuximab vedotina", "Daratumumab", "Isatuximab",
    "Bortezomib", "Carfilzomib", "Ixazomib",
    "Lenalidomida", "Pomalidomida", "Talidomida",
    "Sacituzumab govitecan", "Enfortumab vedotina"
  ),
  immuno = c(
    "Pembrolizumab", "Nivolumab", "Cemiplimab", "Dostarlimab",
    "Atezolizumab", "Durvalumab", "Avelumab",
    "Ipilimumab", "Tremelimumab", "Relatlimab",
    "Tislelizumab", "Toripalimab", "Retifanlimab",
    "BCG intravesical", "Talimogene laherparepvec (T-VEC)",
    "Tisagenlecleucel (CAR-T)", "Axicabtagene ciloleucel (CAR-T)",
    "Brexucabtagene autoleucel (CAR-T)", "Lisocabtagene maraleucel (CAR-T)",
    "Idecabtagene vicleucel (CAR-T)", "Ciltacabtagene autoleucel (CAR-T)",
    "Blinatumomab", "Mosunetuzumab", "Glofitamab", "Epcoritamab",
    "Teclistamab", "Talquetamab", "Elranatamab",
    "Interferon alfa-2b", "IL-2 (aldesleukina)"
  )
)

# ---------------------------------------------------------------------------
# Per-category drug catalog
# ---------------------------------------------------------------------------

THERAPY_DRUGS <- list(

  breast = list(
    hormonal = c("Tamoxifeno", "Anastrozol", "Letrozol", "Exemestano",
                 "Fulvestrant", "Leuprolide", "Goserelina", "Triptorelina",
                 "Megestrol"),
    targeted = c("Trastuzumab", "Pertuzumab",
                 "T-DM1 (ado-trastuzumab emtansina)",
                 "T-DXd (trastuzumab deruxtecan)",
                 "Lapatinib", "Tucatinib", "Neratinib",
                 "Palbociclib", "Ribociclib", "Abemaciclib",
                 "Everolimus", "Alpelisib", "Capivasertib",
                 "Olaparib", "Talazoparib",
                 "Sacituzumab govitecan", "Bevacizumab",
                 "Elacestrant"),
    immuno   = c("Pembrolizumab", "Atezolizumab")
  ),

  colorectal = list(
    hormonal = character(0),
    targeted = c("Bevacizumab", "Aflibercept", "Ramucirumab",
                 "Cetuximab", "Panitumumab",
                 "Regorafenib", "Trifluridina-tipiracil",
                 "Encorafenib + cetuximab (BRAF V600E)",
                 "Sotorasib (KRAS G12C)", "Adagrasib (KRAS G12C)",
                 "Trastuzumab + pertuzumab (HER2+)",
                 "Tucatinib + trastuzumab (HER2+)",
                 "Larotrectinib (NTRK)", "Entrectinib (NTRK)",
                 "Selpercatinib (RET)", "Fruquintinib"),
    immuno   = c("Pembrolizumab (MSI-H/dMMR)", "Nivolumab (MSI-H/dMMR)",
                 "Ipilimumab + nivolumab (MSI-H/dMMR)",
                 "Dostarlimab (MSI-H/dMMR)")
  ),

  lung_nsclc = list(
    hormonal = character(0),
    targeted = c("Osimertinib (EGFR)", "Erlotinib (EGFR)", "Gefitinib (EGFR)",
                 "Afatinib (EGFR)", "Dacomitinib (EGFR)",
                 "Amivantamab (EGFR exon 20)", "Mobocertinib (EGFR exon 20)",
                 "Alectinib (ALK)", "Brigatinib (ALK)", "Lorlatinib (ALK)",
                 "Crizotinib (ALK / ROS1)", "Ceritinib (ALK)",
                 "Entrectinib (ROS1 / NTRK)", "Repotrectinib (ROS1)",
                 "Larotrectinib (NTRK)",
                 "Dabrafenib + trametinib (BRAF V600E)",
                 "Capmatinib (METex14)", "Tepotinib (METex14)",
                 "Selpercatinib (RET)", "Pralsetinib (RET)",
                 "Sotorasib (KRAS G12C)", "Adagrasib (KRAS G12C)",
                 "Trastuzumab deruxtecan (HER2)",
                 "Bevacizumab", "Ramucirumab", "Necitumumab"),
    immuno   = c("Pembrolizumab", "Nivolumab", "Cemiplimab",
                 "Atezolizumab", "Durvalumab",
                 "Ipilimumab + nivolumab",
                 "Tremelimumab + durvalumab",
                 "Tislelizumab")
  ),

  lung_sclc = list(
    hormonal = character(0),
    targeted = c("Lurbinectedina", "Topotecan"),
    immuno   = c("Atezolizumab", "Durvalumab", "Pembrolizumab",
                 "Nivolumab", "Tarlatamab")
  ),

  prostate = list(
    hormonal = c("Leuprolide", "Goserelina", "Triptorelina", "Degarelix",
                 "Relugolix", "Bicalutamida", "Flutamida", "Nilutamida",
                 "Enzalutamida", "Apalutamida", "Darolutamida",
                 "Abiraterona + prednisona"),
    targeted = c("Olaparib (BRCA/HRR)", "Rucaparib (BRCA)",
                 "Talazoparib + enzalutamida (HRR)",
                 "Niraparib + abiraterona (BRCA)",
                 "Lutetio-177-PSMA-617", "Radio-223",
                 "Cabazitaxel"),
    immuno   = c("Pembrolizumab (MSI-H / TMB-H)",
                 "Sipuleucel-T")
  ),

  gyn_cervical = list(
    hormonal = character(0),
    targeted = c("Bevacizumab", "Tisotumab vedotina"),
    immuno   = c("Pembrolizumab", "Cemiplimab", "Nivolumab")
  ),

  gyn_ovarian = list(
    hormonal = c("Letrozol", "Anastrozol", "Tamoxifeno",
                 "Acetato de medroxiprogesterona"),
    targeted = c("Bevacizumab",
                 "Olaparib", "Niraparib", "Rucaparib",
                 "Mirvetuximab soravtansina (FRalpha+)"),
    immuno   = c("Dostarlimab (dMMR)", "Pembrolizumab (MSI-H / TMB-H)")
  ),

  gyn_endometrial = list(
    hormonal = c("Acetato de medroxiprogesterona", "Megestrol",
                 "Tamoxifeno", "Letrozol", "Anastrozol", "Leuprolide"),
    targeted = c("Lenvatinib + pembrolizumab", "Trastuzumab (HER2+)",
                 "Selinexor (mantenimiento p53wt)"),
    immuno   = c("Dostarlimab", "Pembrolizumab", "Lenvatinib + pembrolizumab")
  ),

  vulvar = list(
    hormonal = character(0),
    targeted = c("Cetuximab", "Bevacizumab"),
    immuno   = c("Pembrolizumab", "Cemiplimab", "Nivolumab")
  ),

  penile = list(
    hormonal = character(0),
    targeted = c("Cetuximab", "Panitumumab"),
    immuno   = c("Pembrolizumab")
  ),

  thyroid = list(
    hormonal = c("Levotiroxina supresiva (TSH)"),
    targeted = c("Sorafenib", "Lenvatinib", "Cabozantinib",
                 "Vandetanib", "Selpercatinib (RET)", "Pralsetinib (RET)",
                 "Larotrectinib (NTRK)", "Entrectinib (NTRK)",
                 "Dabrafenib + trametinib (BRAF V600E + anaplasico)"),
    immuno   = c("Pembrolizumab (MSI-H / TMB-H)")
  ),

  melanoma = list(
    hormonal = character(0),
    targeted = c("Vemurafenib", "Dabrafenib", "Encorafenib",
                 "Trametinib", "Cobimetinib", "Binimetinib",
                 "Imatinib (KIT mutado)"),
    immuno   = c("Pembrolizumab", "Nivolumab",
                 "Ipilimumab", "Ipilimumab + nivolumab",
                 "Nivolumab + relatlimab",
                 "Talimogene laherparepvec (T-VEC)",
                 "Lifileucel (TIL)")
  ),

  lymphoma = list(
    hormonal = character(0),
    targeted = c("Rituximab", "Obinutuzumab", "Ofatumumab",
                 "Polatuzumab vedotina", "Brentuximab vedotina",
                 "Loncastuximab tesirina",
                 "Tafasitamab + lenalidomida",
                 "Ibrutinib", "Acalabrutinib", "Zanubrutinib",
                 "Venetoclax", "Idelalisib", "Duvelisib", "Copanlisib",
                 "Selinexor", "Mogamulizumab",
                 "Lenalidomida"),
    immuno   = c("Pembrolizumab (Hodgkin)", "Nivolumab (Hodgkin)",
                 "Tisagenlecleucel (CAR-T)", "Axicabtagene ciloleucel (CAR-T)",
                 "Lisocabtagene maraleucel (CAR-T)",
                 "Brexucabtagene autoleucel (CAR-T MCL)",
                 "Mosunetuzumab", "Glofitamab", "Epcoritamab")
  ),

  sarcoma = list(
    hormonal = character(0),
    targeted = c("Imatinib (GIST)", "Sunitinib (GIST)", "Regorafenib (GIST)",
                 "Avapritinib (GIST PDGFRA)", "Ripretinib (GIST)",
                 "Pazopanib (sarcoma de partes blandas)",
                 "Trabectedina", "Eribulina",
                 "Larotrectinib (NTRK)", "Entrectinib (NTRK)",
                 "Tazemetostat (epitelioide INI1-)",
                 "Crizotinib (ALK)",
                 "Olaratumab"),
    immuno   = c("Pembrolizumab (TMB-H / desmoplasico)",
                 "Nivolumab + ipilimumab (alveolar / epitelioide)")
  ),

  gastric = list(
    hormonal = character(0),
    targeted = c("Trastuzumab (HER2+)", "Trastuzumab deruxtecan (HER2+)",
                 "Ramucirumab", "Bevacizumab",
                 "Zolbetuximab (CLDN18.2)",
                 "Regorafenib", "Trifluridina-tipiracil"),
    immuno   = c("Nivolumab", "Pembrolizumab",
                 "Ipilimumab + nivolumab",
                 "Tislelizumab")
  ),

  hcc = list(
    hormonal = character(0),
    targeted = c("Sorafenib", "Lenvatinib",
                 "Regorafenib", "Cabozantinib", "Ramucirumab (AFP>=400)",
                 "Bevacizumab + atezolizumab"),
    immuno   = c("Atezolizumab + bevacizumab",
                 "Durvalumab + tremelimumab",
                 "Nivolumab", "Pembrolizumab",
                 "Ipilimumab + nivolumab")
  ),

  pancreas = list(
    hormonal = character(0),
    targeted = c("Erlotinib", "Olaparib (BRCA)",
                 "Larotrectinib (NTRK)", "Entrectinib (NTRK)",
                 "Selpercatinib (RET)",
                 "Sotorasib (KRAS G12C)", "Adagrasib (KRAS G12C)",
                 "Trastuzumab (HER2+)"),
    immuno   = c("Pembrolizumab (MSI-H / dMMR)",
                 "Dostarlimab (MSI-H)")
  ),

  head_neck = list(
    hormonal = character(0),
    targeted = c("Cetuximab", "Afatinib"),
    immuno   = c("Pembrolizumab", "Nivolumab")
  ),

  renal = list(
    hormonal = character(0),
    targeted = c("Sunitinib", "Pazopanib", "Cabozantinib", "Axitinib",
                 "Lenvatinib", "Tivozanib", "Sorafenib",
                 "Everolimus", "Temsirolimus",
                 "Belzutifan (HIF-2alpha)",
                 "Bevacizumab + interferon"),
    immuno   = c("Pembrolizumab + axitinib",
                 "Pembrolizumab + lenvatinib",
                 "Avelumab + axitinib",
                 "Nivolumab + ipilimumab",
                 "Nivolumab + cabozantinib",
                 "Nivolumab", "Pembrolizumab")
  ),

  bladder = list(
    hormonal = character(0),
    targeted = c("Erdafitinib (FGFR)", "Enfortumab vedotina (Nectin-4)",
                 "Sacituzumab govitecan",
                 "Trastuzumab deruxtecan (HER2+)",
                 "Disitamab vedotina (HER2+)"),
    immuno   = c("Pembrolizumab", "Nivolumab", "Atezolizumab",
                 "Avelumab (mantenimiento)",
                 "Durvalumab",
                 "BCG intravesical",
                 "Nivolumab + ipilimumab",
                 "Enfortumab vedotina + pembrolizumab")
  ),

  cns_glioma = list(
    hormonal = character(0),
    targeted = c("Bevacizumab",
                 "Vorasidenib (IDH1/2)",
                 "Dabrafenib + trametinib (BRAF V600E)",
                 "Larotrectinib / entrectinib (NTRK)",
                 "Selumetinib (NF1 pediatrico)",
                 "Tovorafenib (pediatrico)"),
    immuno   = character(0)
  ),

  leukemia = list(
    hormonal = character(0),
    targeted = c("Imatinib (CML / Ph+)", "Dasatinib", "Nilotinib",
                 "Bosutinib", "Ponatinib", "Asciminib",
                 "Venetoclax + azacitidina (AML)",
                 "Midostaurina (FLT3)", "Gilteritinib (FLT3)",
                 "Quizartinib (FLT3-ITD)",
                 "Ivosidenib (IDH1)", "Enasidenib (IDH2)",
                 "Glasdegib (AML)",
                 "Gemtuzumab ozogamicina (CD33+)",
                 "Inotuzumab ozogamicina (B-ALL)",
                 "Ibrutinib (CLL)", "Acalabrutinib (CLL)",
                 "Zanubrutinib (CLL)", "Pirtobrutinib (CLL)",
                 "Idelalisib", "Duvelisib", "Venetoclax (CLL)",
                 "Obinutuzumab (CLL)", "Rituximab (CLL)",
                 "Ruxolitinib (MPN)", "Fedratinib (MF)", "Pacritinib (MF)",
                 "Momelotinib (MF)"),
    immuno   = c("Blinatumomab (B-ALL CD19)",
                 "Tisagenlecleucel (CAR-T B-ALL)",
                 "Brexucabtagene autoleucel (CAR-T B-ALL)",
                 "Pembrolizumab (cHL post-trasplante)")
  ),

  myeloma = list(
    hormonal = c("Dexametasona (parte del esquema)"),
    targeted = c("Bortezomib", "Carfilzomib", "Ixazomib",
                 "Lenalidomida", "Pomalidomida", "Talidomida",
                 "Daratumumab", "Isatuximab",
                 "Elotuzumab", "Selinexor",
                 "Belantamab mafodotina (BCMA)",
                 "Venetoclax (t(11;14))"),
    immuno   = c("Idecabtagene vicleucel (CAR-T BCMA)",
                 "Ciltacabtagene autoleucel (CAR-T BCMA)",
                 "Teclistamab (BCMAxCD3)",
                 "Talquetamab (GPRC5DxCD3)",
                 "Elranatamab (BCMAxCD3)")
  ),

  appendix = list(
    hormonal = c("Octreotide LAR (carcinoide)", "Lanreotide (carcinoide)"),
    targeted = c("Bevacizumab", "Cetuximab", "Panitumumab"),
    immuno   = c("Pembrolizumab (MSI-H / dMMR)")
  ),

  anal = list(
    hormonal = character(0),
    targeted = c("Cetuximab"),
    immuno   = c("Nivolumab", "Pembrolizumab", "Retifanlimab")
  ),

  testis = list(
    hormonal = character(0),
    targeted = c("Pazopanib (rescate selecto)"),
    immuno   = character(0)
  ),

  esophagus = list(
    hormonal = character(0),
    targeted = c("Trastuzumab (HER2+)", "Trastuzumab deruxtecan (HER2+)",
                 "Ramucirumab",
                 "Zolbetuximab (CLDN18.2, GEJ)"),
    immuno   = c("Pembrolizumab", "Nivolumab",
                 "Ipilimumab + nivolumab",
                 "Tislelizumab",
                 "Durvalumab + tremelimumab")
  ),

  ampullary = list(
    hormonal = character(0),
    targeted = c("Erlotinib (pancreatobiliar)",
                 "Cetuximab (intestinal)", "Panitumumab (intestinal)",
                 "Bevacizumab"),
    immuno   = c("Pembrolizumab (MSI-H / dMMR)",
                 "Dostarlimab (MSI-H)")
  ),

  biliary = list(
    hormonal = character(0),
    targeted = c("Ivosidenib (IDH1)",
                 "Pemigatinib (FGFR2)", "Infigratinib (FGFR2)",
                 "Futibatinib (FGFR2)",
                 "Trastuzumab + pertuzumab (HER2+)",
                 "Trastuzumab deruxtecan (HER2+)",
                 "Dabrafenib + trametinib (BRAF V600E)",
                 "Larotrectinib (NTRK)", "Entrectinib (NTRK)",
                 "Selpercatinib (RET)"),
    immuno   = c("Durvalumab + gemcitabina-cisplatino",
                 "Pembrolizumab + gemcitabina-cisplatino",
                 "Nivolumab",
                 "Pembrolizumab (MSI-H / TMB-H)")
  ),

  salivary = list(
    hormonal = c("Bicalutamida (carcinoma ductal AR+)",
                 "Leuprolide (AR+)"),
    targeted = c("Trastuzumab + pertuzumab (HER2+)",
                 "T-DM1 (HER2+)",
                 "Larotrectinib / entrectinib (NTRK -- secretor)",
                 "Selpercatinib (RET)", "Pralsetinib (RET)",
                 "Sorafenib", "Lenvatinib", "Axitinib"),
    immuno   = c("Pembrolizumab (PD-L1+ / TMB-H)")
  ),

  mesothelioma = list(
    hormonal = character(0),
    targeted = c("Bevacizumab"),
    immuno   = c("Nivolumab + ipilimumab",
                 "Pembrolizumab",
                 "Atezolizumab + bevacizumab")
  ),

  thymoma = list(
    hormonal = c("Octreotide LAR + prednisona"),
    targeted = c("Sunitinib (carcinoma timico)",
                 "Lenvatinib"),
    immuno   = c("Pembrolizumab (carcinoma timico, vigilar miastenia)")
  ),

  net = list(
    hormonal = c("Octreotide LAR", "Lanreotide", "Pasireotide LAR"),
    targeted = c("Everolimus", "Sunitinib (pancreatico)",
                 "Cabozantinib", "Belzutifan (VHL)",
                 "Telotristat (sintomatico carcinoide)",
                 "Lutetio-177-DOTATATE (PRRT)"),
    immuno   = c("Pembrolizumab (NEC G3 MSI-H / TMB-H)",
                 "Nivolumab + ipilimumab (NEC selecto)")
  ),

  cup = list(
    hormonal = character(0),
    targeted = c("Trastuzumab (HER2+)", "Cetuximab", "Panitumumab",
                 "Bevacizumab",
                 "Larotrectinib / entrectinib (NTRK)",
                 "Selpercatinib (RET)", "Pralsetinib (RET)",
                 "Sotorasib (KRAS G12C)",
                 "Dabrafenib + trametinib (BRAF V600E)",
                 "Crizotinib / alectinib (ALK)"),
    immuno   = c("Pembrolizumab (MSI-H / TMB-H / PD-L1+)",
                 "Nivolumab + ipilimumab")
  ),

  other = list(
    hormonal = .therapy_generic$hormonal,
    targeted = .therapy_generic$targeted,
    immuno   = .therapy_generic$immuno
  )
)

# ---------------------------------------------------------------------------
# Lookup helper
# ---------------------------------------------------------------------------

#' Get the drug list for a (category, kind) pair.
#'
#' @param category Character cancer category from `cancer_category()`.
#' @param kind     One of "hormonal", "targeted", "immuno".
#' @return Character vector of generic drug names. Falls back to the generic
#'   class list when the category is unknown or has no curated entry.
lookup_therapy_drugs <- function(category, kind = c("hormonal","targeted","immuno")) {
  kind <- match.arg(kind)
  cat_key <- if (is.null(category) || is.na(category) || !nzchar(category)) "other"
             else as.character(category)
  drugs <- THERAPY_DRUGS[[cat_key]][[kind]]
  if (is.null(drugs) || !length(drugs)) drugs <- .therapy_generic[[kind]]
  unique(drugs)
}
