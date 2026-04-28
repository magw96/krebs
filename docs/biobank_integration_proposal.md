# Krebs ↔ Biobanco — Propuesta de integración

**Autor:** Fabián A. Gutiérrez Valle (con asistencia de Claude / Krebs V0.2)
**Fecha:** 2026-04-27
**Versión:** 0.1 (borrador para discusión)

---

## 1. Objetivo

Vincular cada paciente y cada encuentro clínico de **Krebs** con una o más
**muestras biológicas** (tejido tumoral, tejido normal pareado, sangre, plasma,
suero, ADN/ARN extraído, FFPE, OCT/snap-frozen, organoides, líneas celulares
derivadas), de tal manera que:

1. Los datos clínicos no se filtren al banco ni viceversa, salvo a través de
   un **puente pseudonimizado** auditable.
2. La trazabilidad de la muestra cumpla **ISO 20387:2018** y las
   **ISBER Best Practices** (5.ª ed., 2023).
3. El uso secundario de muestras y datos esté gobernado por un **consentimiento
   informado** registrado, versionado y revocable.
4. La arquitectura sea compatible con interoperabilidad estándar
   (**HL7 FHIR Specimen R5**, **GA4GH Phenopackets v2**) para que el banco
   pueda responder a solicitudes de redes nacionales (RNCO, INCan) o
   internacionales (BBMRI-ERIC, ICGC-ARGO, cBioPortal).

---

## 2. Marco regulatorio aplicable (México + internacional)

### 2.1 México

- **Ley General de Salud**, Título Quinto bis (Investigación para la Salud) y
  Título Decimocuarto (Donación de órganos, tejidos y células).
- **Reglamento de la LGS en Materia de Investigación para la Salud**
  (DOF 06/01/1987, última reforma 02/04/2014) — define **biobanco con fines
  de investigación** (art. 41 Bis 1 a 5).
- **NOM-012-SSA3-2012** — criterios para la ejecución de proyectos de
  investigación en seres humanos.
- **NOM-024-SSA3-2012** — Sistemas de Información de Registro Electrónico
  para la Salud (interoperabilidad y seguridad de los datos clínicos).
- **NOM-004-SSA3-2012** — Expediente clínico (acceso, conservación, secreto
  profesional).
- **COFEPRIS** — registro y autorización del biobanco; reporte anual de
  actividad (Disposiciones aplicables a los biobancos, 2018).
- **Ley Federal de Protección de Datos Personales en Posesión de los
  Particulares (LFPDPPP)** y su Reglamento — datos sensibles de salud,
  principios de finalidad, calidad y proporcionalidad.
- **Comisión Nacional de Bioética (CONBIOÉTICA)** — registro y vigencia del
  Comité de Ética en Investigación (CEI) y del Comité de Investigación local.

### 2.2 Internacional / estándares de calidad

| Estándar                          | Para qué sirve                                                             |
|-----------------------------------|----------------------------------------------------------------------------|
| **ISO 20387:2018**                | Requisitos generales para la competencia de biobancos (acreditable).       |
| **ISO 15189:2022**                | Calidad y competencia para laboratorios médicos (si se procesa in-house).  |
| **ISBER Best Practices 5e**       | Procesos operativos de banco (recolección, procesamiento, almacenamiento). |
| **CAP Biorepository Accreditation** | Acreditación del College of American Pathologists.                       |
| **Declaración de Helsinki (WMA)** | Principios éticos para investigación con seres humanos.                    |
| **CIOMS 2016**                    | Pautas éticas internacionales (uso secundario, consentimiento amplio).     |
| **GA4GH** (Phenopackets, DUO, Passports) | Interoperabilidad clínica/genómica y consentimiento computable.       |
| **HL7 FHIR R5 — Specimen / Consent / ResearchSubject** | Modelo de datos para intercambio.                       |
| **GDPR Art. 9 / HIPAA §164.514** | Pertinente si hay colaboradores en UE/EE. UU. (de-identification).         |
| **21 CFR Part 11** (FDA)          | Si el banco abastecerá ensayos clínicos regulados por FDA.                 |
| **OECD Best Practice Guidelines for BRCs (2007)** | Centros de Recursos Biológicos.                            |

> Recomendación: apuntar desde el día 0 a **ISO 20387 + ISBER** como objetivo
> de auditoría interna; CAP/ISO 15189 como meta a 24-36 meses si se procesa
> tejido en sitio.

---

## 3. Modelo de identificación (BIOID)

### 3.1 Principio de separación

```
┌─────────────────────┐      keystore        ┌─────────────────────┐
│   Krebs (clínico)   │  ◄── pseudónimo ──►  │   Biobanco (LIMS)   │
│   patient_id (MRN)  │                      │   bio_subject_id    │
│   datos clínicos    │                      │   muestras          │
└─────────────────────┘                      └─────────────────────┘
            ▲                                          ▲
            │                                          │
            └──────── solo el Custodio (DPO) ──────────┘
                       puede unir ambas tablas
```

- **MRN** vive solo en Krebs.
- **bio_subject_id** vive solo en el banco; es opaco (no derivable de MRN).
- La tabla `subject_link` (sujeto ↔ MRN) vive en una **base separada** con
  cifrado at-rest, accesible solo al Custodio del biobanco / DPO mediante
  cuenta de servicio auditada.
- Krebs *nunca* lee `subject_link` de forma directa: si necesita resolver un
  pseudónimo, manda un job firmado al servicio de pseudonimización.

### 3.2 Esquema de BIOID (compatible con ISBER §C.4)

```
{INST}-{SUBJ}-{COL}-{TYPE}-{ALIQ}
  │      │     │     │      │
  │      │     │     │      └─ alícuota/vial (A01, A02, ...)
  │      │     │     └──────── tipo de muestra (TUM, NORM, BLD, PLA, SER, DNA, RNA, FFPE, OCT, ORG)
  │      │     └────────────── n.º de colecta para ese sujeto (001, 002, ...)
  │      └──────────────────── sujeto pseudónimo (8 chars base32)
  └─────────────────────────── institución / nodo (UP01, INC01, etc.)
```

Ejemplo: `UP01-7K3M9PQ2-003-FFPE-A02`

- `SUBJ` se genera con `HMAC-SHA256(MRN || institution_salt)` truncado y
  codificado en base32 sin caracteres ambiguos (Crockford). Determinístico
  → la misma persona en distintas colectas siempre obtiene el mismo `SUBJ`,
  pero no se puede invertir sin la sal.
- La sal vive en un **secret manager** (AWS KMS / GCP KMS / HashiCorp Vault),
  nunca en código ni en variables de entorno claras.

---

## 4. Modelo de datos (esquema sugerido)

> Postgres separado del de Krebs (`krebs_biobank`), o al menos en otro
> *schema* (`biobank.*`) con su propio rol y RLS.

```sql
-- Sujeto pseudónimo en el banco
CREATE TABLE biobank.subject (
  bio_subject_id  TEXT PRIMARY KEY,           -- p.ej. UP01-7K3M9PQ2
  sex_at_birth    TEXT,
  birth_year      INT,                        -- granularidad k-anonimizable
  consent_id      UUID NOT NULL REFERENCES biobank.consent(id),
  created_at      TIMESTAMPTZ DEFAULT now(),
  created_by      TEXT NOT NULL
);

-- Muestra física
CREATE TABLE biobank.specimen (
  bioid           TEXT PRIMARY KEY,
  bio_subject_id  TEXT NOT NULL REFERENCES biobank.subject(bio_subject_id),
  parent_bioid    TEXT REFERENCES biobank.specimen(bioid),  -- alícuotas / derivados
  collection_dt   TIMESTAMPTZ NOT NULL,
  anatomic_site   TEXT,                       -- ICD-O-3 topo
  morphology      TEXT,                       -- ICD-O-3 morpho
  laterality      TEXT,
  preservation    TEXT CHECK (preservation IN
                  ('FFPE','SNAP_FROZEN','OCT','RNA_LATER','VIABLE','FIXED_OTHER')),
  container       TEXT,                       -- vial, bloque, slide
  storage_temp    TEXT CHECK (storage_temp IN
                  ('RT','+4','-20','-80','-150','-196')),
  freezer         TEXT, rack TEXT, box TEXT, position TEXT,
  volume_uL       NUMERIC,
  mass_mg         NUMERIC,
  conc_ng_uL      NUMERIC,
  rin             NUMERIC,                    -- para RNA
  dv200           NUMERIC,                    -- para FFPE RNA
  qc_passed       BOOLEAN,
  cohort_tag      TEXT,                       -- proyecto/estudio
  status          TEXT CHECK (status IN
                  ('AVAILABLE','RESERVED','SHIPPED','EXHAUSTED','DESTROYED','QUARANTINED')),
  notes           TEXT,
  created_at      TIMESTAMPTZ DEFAULT now()
);

-- Cadena de custodia (append-only)
CREATE TABLE biobank.chain_of_custody (
  id SERIAL PRIMARY KEY,
  bioid           TEXT NOT NULL REFERENCES biobank.specimen(bioid),
  event           TEXT NOT NULL CHECK (event IN
                  ('COLLECTED','RECEIVED','PROCESSED','ALIQUOTED','MOVED',
                   'THAWED','SHIPPED','RETURNED','DESTROYED','QC')),
  event_dt        TIMESTAMPTZ NOT NULL DEFAULT now(),
  actor           TEXT NOT NULL,
  from_location   TEXT, to_location TEXT,
  attachment      TEXT,                       -- foto / hoja de proceso
  notes           TEXT
);

-- Consentimiento (versionado, revocable)
CREATE TABLE biobank.consent (
  id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  bio_subject_id  TEXT NOT NULL REFERENCES biobank.subject(bio_subject_id),
  template_version TEXT NOT NULL,             -- p.ej. "ICF-Krebs-v3.2-2026"
  scope           TEXT CHECK (scope IN ('specific','broad','tiered')),
  permits_genomics BOOLEAN, permits_commercial BOOLEAN,
  permits_intl_share BOOLEAN, permits_recontact BOOLEAN,
  signed_dt       DATE NOT NULL,
  withdrawn_dt    DATE,
  pdf_attachment  TEXT,                       -- ruta a PDF firmado
  irb_protocol    TEXT NOT NULL,              -- registro CEI / COFEPRIS
  created_at      TIMESTAMPTZ DEFAULT now()
);

-- Distribución / solicitudes
CREATE TABLE biobank.request (
  id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  pi_name         TEXT, pi_affiliation TEXT,
  protocol        TEXT NOT NULL, irb_status TEXT,
  mta_signed_dt   DATE,                       -- material transfer agreement
  approved_by     TEXT, approved_dt DATE,
  shipped_dt      DATE
);
CREATE TABLE biobank.request_item (
  request_id UUID REFERENCES biobank.request(id),
  bioid TEXT REFERENCES biobank.specimen(bioid),
  PRIMARY KEY (request_id, bioid)
);
```

### Tabla puente (vive aparte, cifrada)

```sql
CREATE TABLE keystore.subject_link (
  bio_subject_id TEXT PRIMARY KEY,
  mrn_encrypted  BYTEA NOT NULL,    -- cifrado AES-256-GCM con DEK por sujeto
  hospital_id    TEXT NOT NULL,
  linked_at      TIMESTAMPTZ DEFAULT now(),
  linked_by      TEXT NOT NULL
);
```

---

## 5. Flujo operativo Krebs ↔ Banco

```
[1] Encuentro clinico en Krebs
       │  (medico marca: "muestra recolectada para banco")
       ▼
[2] Krebs llama al servicio de pseudonimizacion
       │  (HMAC(mrn||salt) -> bio_subject_id)
       ▼
[3] Servicio responde: bio_subject_id + token de colecta
       │  (token = JWT firmado, vence en 24 h)
       ▼
[4] Etiqueta fisica con BIOID se imprime; se adjunta al vial
       │
       ▼
[5] Tecnico de banco escanea BIOID -> abre flujo en LIMS
       │  (valida token, registra recepcion en chain_of_custody)
       ▼
[6] Banco procesa, alicuota, almacena -> actualiza specimen
       │
       ▼
[7] Investigador solicita -> request -> aprobado por CEI -> shipping
```

Krebs guarda únicamente el **BIOID** y la **fecha de colecta** en su tabla
de encounters (campo `specimen_bioids TEXT[]`); no almacena ubicación ni
estado del vial. Si el clínico necesita saber qué hay en el banco para ese
paciente, Krebs hace una llamada autenticada al API del LIMS:
`GET /specimens?bio_subject_id=...` (con token de scope `clinical_read`).

---

## 6. Implementación por fases

### Fase 0 (4-6 semanas) — Gobernanza
- [ ] Inventario de protocolos y consentimientos vigentes.
- [ ] Definir Custodio del biobanco (rol y persona física).
- [ ] Registrar el biobanco ante COFEPRIS.
- [ ] Acreditar/renovar CEI y CI institucionales.
- [ ] Redactar **SOPs** mínimos (recolección, recepción, alícuota,
      almacenamiento, distribución, destrucción, contingencia eléctrica).
- [ ] Aprobar el modelo de **consentimiento amplio escalonado** (tiered).

### Fase 1 (8-12 semanas) — MVP técnico
- [ ] Esquema Postgres `biobank.*` + `keystore.*` (otro servidor o RDS).
- [ ] Servicio de pseudonimización (FastAPI o plumber R) con KMS.
- [ ] Módulo Shiny `mod_biobank_request` dentro de Krebs:
      - el clínico marca la muestra en el encuentro;
      - genera BIOID + etiqueta PDF imprimible (Code128 / DataMatrix).
- [ ] Vista Shiny `mod_biobank_inventory` (rol técnico de banco):
      cadena de custodia, búsqueda por BIOID, mover viales, QC.
- [ ] Audit log append-only (ya existe patrón en Krebs).

### Fase 2 (3-6 meses) — LIMS dedicado
- [ ] Evaluar **OpenSpecimen** (Java, open source, ISBER-aligned) como LIMS;
      Krebs queda como front clínico, OpenSpecimen como backend de banco.
- [ ] Sincronización vía FHIR Specimen + `Consent`.
- [ ] Validación analítica (RIN/DV200, control de temperatura con loggers).

### Fase 3 (6-12 meses) — Acreditación e interoperabilidad
- [ ] Auditoría interna ISO 20387.
- [ ] Conexión a **BBMRI Directory** (o nodo nacional) para descubrimiento
      por terceros sin exponer pacientes.
- [ ] Plantillas de **Phenopackets** generadas desde Krebs por sujeto.
- [ ] Política de retorno de hallazgos (re-contacto vía CEI).

---

## 7. Decisiones que necesito de ti

1. **Institución de origen** (nodo `INST` del BIOID): UP, INCan, otra?
2. **Quién será Custodio del banco** y bajo qué CEI se va a registrar.
3. **¿LIMS dedicado (OpenSpecimen) o todo dentro de Krebs?**
   - Krebs-only: más rápido, menos features, requiere validar ISO en Krebs.
   - OpenSpecimen: más alineado con ISBER, más infra que mantener.
4. **Modelo de consentimiento**: específico, amplio, o escalonado (tiered).
5. **Tipos de muestra prioritarios** para empezar (FFPE? snap-frozen? sangre?).
6. **Plan de almacenamiento físico**: -80, -150 (vapor de N2), -196 (líquido).
7. **¿Sale del país?** Si sí, hay que diseñar MTAs y cláusulas
   GDPR-compatibles desde el día 0.

Cuando me confirmes 1-3 levanto el módulo `mod_biobank_request` con BIOID
en Krebs y el `keystore` separado, y la migración SQL `009_biobank.sql`.
