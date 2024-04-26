# Try querying Australian Plant Trait Dictionary (APD)
# Use pacakge 'glitter' for the sake of simplicity
library("glitter")

# Setup ------------------------------------------------------------------------
query_basis = spq_init(
  endpoint = paste0(
    "http://vocabs.ardc.edu.au/repository/api/sparql/",
    "austraits_austraits-plant-dictionary_",
    "austraits-plant-dictionary-version-1-1-0"
  ),
  request_control = spq_control_request(
    request_type = "body-form"
  )
)

query_with_prefixes = query_basis %>%
  spq_prefix(
    prefixes = c(
      rdf          = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      APD          = "https://w3id.org/APD/traits/",
      APD_glossary = "https://w3id.org/APD/glossary/",
      dc           = "http://purl.org/dc/elements/1.1/",
      skos         = "http://www.w3.org/2004/02/skos/core#",
      dwc          = "http://rs.tdwg.org/dwc/terms/attributes/",
      dcam         = "http://purl.org/dc/dcam/",
      dcterms      = "http://purl.org/dc/terms/",
      ets          = "http://terminologies.gfbio.org/terms/ETS/",
      obo          = "http://purl.obolibrary.org/obo/",
      oboecore     = "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#",
      ont          = "https://w3id.org/iadopt/ont/",
      owl          = "http://www.w3.org/2002/07/owl#",
      rdfs         = "http://www.w3.org/2000/01/rdf-schema#",
      uom          = "https://w3id.org/uom/",
      datacite     = "http://purl.org/datacite/v4.4/",
      xsd          = "http://www.w3.org/2001/XMLSchema#",
      Cerrado      = "http://cerrado.linkeddata.es/ecology/",
      CorVeg       = "http://linked.data.gov.au/def/corveg-cv/",
      DCM          = "http://dicom.nema.org/resources/ontology/DCM/",
      EDAM         = "http://edamontology.org/",
      EFO          = "http://www.ebi.ac.uk/efo/",
      EnvThes      = "http://vocabs.lter-europe.net/EnvThes/",
      hupson       = "http://scai.fraunhofer.de/HuPSON#",
      IOBC         = "http://purl.jp/bio/4/id/",
      MESH         = "http://purl.bioontology.org/ontology/MESH/",
      odo          = "http://purl.dataone.org/odo/",
      ORCID        = "https://orcid.org/",
      SIO          = "http://semanticscience.org/resource/",
      SWEET        = "http://sweetontology.net/"
    )
  )


# Example queries --------------------------------------------------------------
# Basic query
apd_basic = query_basis %>%
  spq_add("?s ?p ?o") %>%
  spq_head(n = 10) %>%
  spq_perform()

apd_basic


# Find anything that has name "Leaf boron" in it
query_basis %>%
  spq_add("?s ?p ?o") %>%
  spq_filter(str_detect(o, "Leaf boron")) %>%
  spq_select(-s, .spq_duplicate = "distinct") %>%
  spq_head(10) %>%
  spq_perform()


# Get all continuous traits
query_with_prefixes %>%
  spq_add("?trait_uri rdfs:label ?label") %>%
  spq_add("?trait_uri ets:valueType obo:STATO_0000251") %>%
  spq_perform()


# Actual extraction ------------------------------------------------------------

# All traits that have exact matches in other databases
query_with_prefixes %>%
  spq_add("?trait_uri rdfs:label ?trait_label") %>%
  spq_add("?trait_uri skos:altLabel ?alternative_label") %>%
  spq_add("?trait_uri skos:exactMatch ?exact_match") %>%
  spq_perform()

# All traits that may have or not close, exact or related matches
trait_equivalents = query_with_prefixes %>%
  spq_add("?trait_uri rdfs:label ?trait_label") %>%
  spq_add("?trait_uri skos:altLabel ?alternative_label") %>%
  spq_add("?trait_uri skos:exactMatch ?exact_match", .required = FALSE) %>%
  spq_add("?trait_uri skos:closeMatch ?close_match", .required = FALSE) %>%
  spq_add("?trait_uri skos:relatedMatch ?related_match", .required = FALSE) %>%
  spq_perform()

# Give trait categories and context
trait_categories = query_with_prefixes %>%
  spq_add("?trait_uri rdfs:label ?trait_label") %>%
  spq_add("?trait_uri skos:altLabel ?alternative_label") %>%
  spq_add("?trait_uri skos:broader ?broader_trait", .required = FALSE) %>%
  spq_label(broader_trait) %>%
  spq_add(
    "?trait_uri ont:hasContextObject ?trait_context", .required = FALSE
  ) %>%
  spq_label(trait_context) %>%
  spq_perform()

# All categories inheriting plant traits
# 'skos:broader+' means all subject AND THEIR children inheriting from
# 'APD:trait_group_0000000' = plant traits
trait_groups =  query_with_prefixes |>
  spq_add("?uris skos:broader+ APD:trait_group_0000000") |>
  spq_label(uris) |>
  spq_add("?uris dcterms:identifier ?id", .required = FALSE) |>
  # Keep only the ones that have 'trait_group' in them
  spq_filter(spq('regex(?id, \"trait_group\")')) |>
  spq_perform()

query_with_prefixes |>
  spq_add("?trait_uri rdfs:label ?trait_label") |>
  spq_add("?trait_uri skos:altLabel ?alternative_label") |>
  spq_add("?trait_uri ont:hasContextObject obo:PO_0025034") |>
  spq_add("?trait_uri dcterms:identifier APD:trait_0010111") |>
  spq_perform()
