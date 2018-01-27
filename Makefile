all: prolog/rdf_owl/owl.pl

test:
	swipl -l tests/tests.pl -g run_tests,halt

ontologies/owl.rdf: ontologies/owl-edit.ttl
	riot --output=rdfxml  $< > $@
ontologies/owlrdfs.rdf: ontologies/owl.rdf ontologies/rdfs.rdf
	riot --output=rdfxml $^  > $@

prolog/rdf_owl/owl.pl: ontologies/owlrdfs.rdf
	rdfs2pl owl $< > $@

