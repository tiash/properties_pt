Value Parsetransform
====================

This parse transform to defined properties modules.

The generated modules will provide functions to get the current value of the defined properties.

Warning: This parse transform will completly discard the original code!.

Additionally the functions are also exported:

* `reload()` load the properties from their source,<br>
  only available if runtime code generation has not been disabled.

* `set(Key,Value)`, `set({Key,Value})` `set([{Key,Value}])` updates the config
  (specifically the application environment)
  only available if the first source is an application.

* `get()`, `get(Key)` utility accessors for the configuration.

* `Key()` accessor for the properties.

Usage
-----
* add `-compile([{parse_transform,properties_pt}])` to your code.

* add `-property({Name,Default})` or `-property(Name)` for each property.

* optionally add one (or more) to specify where to get the property values (checked in the order given)
    -source({application,App}).
      % use application:get_env(App,KEY)
    -source({file,File}).
      % uses proplists:lookup(KEY,element(2,file:consult(File)))
    -source({proplists,Props}).
      % uses proplists:lookup(KEY,Props)
    -source({static,Source}).
      % like -source(Source) but ONLY evaluated at compile time
  see `properties_pt:value/2` for more details.

* optionally any of the following options:
    -compile({properties_pt,[no_codegen]}).
      % disable the runtime codegeneration completely
    -compile({properties_pt,[no_onload]}).
      % disable the on_load hook completly
    -compile({properties_pt,[static]}). 
      % evaluate all sources at compile time (will still be re-evaluated during reload)

Tips
----
* For compile time properties (suitable for config files)
    -compile({properties_pt,[no_codegen,static]}).

* For precompiled defaults and updating at runtime (suitable for config files)
    -compile({properties_pt,[static,no_onload]).

* For runtime only properties (suitable for application properties and)
    -compile({properties_pt,[no_codegen]).

Notes
-----
If runtime code generation is enabled (default) then `properties_pt` must be on
the runtime code path, as must the `Syntax_Tools` and `Compiler` applications.

If static is used application properties will likely be wrong, no attempt is
made to load the required application(s).

Todos
-----
Allow more complicated sources; particularly to consult functions, and select nested properties.


