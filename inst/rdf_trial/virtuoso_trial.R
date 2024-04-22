# Try using RDF and Semantic Information based on Virtuoso
# A DBMS for Semantic data to make query faster
library("virtuoso")

vos_start()
con = vos_connect()

vos_import(con, "inst/exdata/apd/APD.ttl")

# Get all continuous traits
vos_query(
  con,
  "
PREFIX APD: <https://w3id.org/APD/traits/>
PREFIX APD_glossary: <https://w3id.org/APD/glossary/>
PREFIX dc: <http://purl.org/dc/elements/1.1/>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX dwc: <http://rs.tdwg.org/dwc/terms/attributes/>
PREFIX dcam: <http://purl.org/dc/dcam/>
PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX ets: <http://terminologies.gfbio.org/terms/ETS/>
PREFIX obo: <http://purl.obolibrary.org/obo/>
PREFIX oboecore: <http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#>
PREFIX ont: <https://w3id.org/iadopt/ont/>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX uom: <https://w3id.org/uom/>
PREFIX datacite: <http://purl.org/datacite/v4.4/>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX Cerrado: <http://cerrado.linkeddata.es/ecology/>
PREFIX CorVeg: <http://linked.data.gov.au/def/corveg-cv/>
PREFIX DCM: <http://dicom.nema.org/resources/ontology/DCM/>
PREFIX EDAM: <http://edamontology.org/>
PREFIX EFO: <http://www.ebi.ac.uk/efo/>
PREFIX EnvThes: <http://vocabs.lter-europe.net/EnvThes/>
PREFIX hupson: <http://scai.fraunhofer.de/HuPSON#>
PREFIX IOBC: <http://purl.jp/bio/4/id/>
PREFIX MESH: <http://purl.bioontology.org/ontology/MESH/>
PREFIX odo: <http://purl.dataone.org/odo/>
PREFIX ORCID: <https://orcid.org/>
PREFIX SIO: <http://semanticscience.org/resource/>
PREFIX SWEET: <http://sweetontology.net/>
SELECT * WHERE {
  ?sub rdfs:label ?label.
  ?sub ets:valueType obo:STATO_0000251.
}"
)

vos_kill()
