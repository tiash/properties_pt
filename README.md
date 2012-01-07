Value Parsetransform
====================

This parse transform to defined properties modules.

The generated modules will provide functions to get the current value of the defined properties.

Warning: This parse transform will completly discard the original code!.

Additionally the functions are also exported:
* `reload()` load the properties from their source.
* `set(Key,Value)`, `set({Key,Value})` `set([{Key,Value}])` updates the config (it will change the config of the last application specified).
* `get()`, `get(Key)` gets the underlying config.
* `Key()` gets the property.

Usage
-----
1. add `-compile([{parse_transform,properties_pt}])` to your code.
2. add `-property({Name,Default})` or `-property(Name)` for each property.
% 3. optionally add `-source({file,File})` or `-source({application,App})` or `-source({M,F,A})`.
3. optionally add one (or more) of:
   `-compile({properties_pt,[{application,App}]})`.
   `-compile({properties_pt,[{file,File}]})`.
4. optionally add `-compile({properties_pt,[no_codegen]})` (reload will be a null op, and the module will not recompile in response to changed propertie values).

