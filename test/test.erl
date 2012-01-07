-module(test).

-compile({parse_transform,properties_pt}).

-source({application,sasl}).
-source({file,"test/test.config"}).

-property(errlog_type).
-property(blah).
-property({ping,pong}).


