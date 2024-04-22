# RDFlib trial
library("rdflib")

apd = rdflib::rdf_parse("inst/exdata/apd/APD.ttl", "turtle")

# Get anything that mentions 'leaf boron'
rdf_query(
  apd,
  'SELECT DISTINCT ?o ?p
   WHERE {

   ?s ?p ?o.
   FILTER(REGEX(?o,"Leaf boron"))
   }

   LIMIT 10'
)

# Get anything that mentions '0012010'
rdf_query(
  apd,
  'SELECT DISTINCT ?s ?o ?p
   WHERE {

   ?s ?p ?o.
   FILTER(REGEX(?o,"0012010"))
   }

   LIMIT 10'
)

# Get all the traits that would have a maximum allowed value of 1000
rdf_query(
   apd,
   'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
   PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
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
   SELECT ?trait_uri ?label
   WHERE {
    ?trait_uri rdfs:label ?label;
       <http://terminologies.gfbio.org/terms/ETS/maxAllowedValue> 1000.
   }
  '
)

# Get all continuous traits
hu = '
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
}'

continuous_traits = rdf_query(apd,hu)

# Continous traits that have equivalents in other databases
rdf_query(apd, "
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
  ?sub rdfs:label ?label ;
       skos:altLabel ?alternative_label ;
       skos:exactMatch ?exact_match ;
       skos:closeMatch ?close_match ;
       skos:relatedMatch ?related_match.
}
LIMIT 10
")
