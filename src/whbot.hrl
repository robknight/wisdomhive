

-define(FIELD(Type, Var, Label, Value), 
	exmpp_xml:element(?NS_DATA_FORMS, 'field', [?XMLATTR('type', Type), ?XMLATTR('var', Var), ?XMLATTR('label', Label) ],
				[exmpp_xml:element(?NS_DATA_FORMS, 'value', [], [?XMLCDATA(Value)])])).
