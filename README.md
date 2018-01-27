# rdf_owl

OWL Library layered on top of SWI-Prolog semweb RDF library

## Comparison to Thea2

An alternate approach to using OWL from Prolog is Thea2. Broadly
speaking, Thea was designed for working with complex TBoxes. rdf_owl
is designed for RDF applications that want to add additional
convenience for OWL TBox constructs.

 * In Thea, OWL axioms are unit clauses. rdf_owl is layered on rdf/3 and rdf/4 triples
 * In Thea, URIs are fully expanded. With rdf_owl, rdf_register_prefix/2 can be used to allow shortforms
 * Thea has a complete direct representation of OWL2. rdf_owl so far only has a subset


An example:

Querying for X subClassOf part-of some Y in Thea:

```
? PART_OF='http://purl.obolibrary.org/obo/BFO_0000050',
SubClassOf(X,someValuesFrom(PART_OF,Y))
```

Same thing with rdf_owl

```
:- rdf_register_prefix(part_of,'http://purl.obolibrary.org/obo/BFO_0000050').

? subClassOf(X,Restr),owl_some(Restr,part_of:'',Y)
```
