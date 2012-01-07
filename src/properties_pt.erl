-module(properties_pt).

-export([parse_transform/2]).
-export([reload/3]).

-undef(DEBUG).
% -define(DEBUG,true).
-include("debug.hrl").

-include("parsetransform.hrl").

parse_transform(Forms0,COptions) ->
  Forms1 = ?forms(Forms0),
  Module = getModule(Forms1),
  Properties = getProperties(Forms1),
  Options = lists:flatten([ Opt ||  {?MODULE,Opt} <- proplists:get_all_values(?MODULE,COptions) ]),
  Sources = getSources(Forms1),
  GenCode = not(proplists:get_bool(no_codegen,Options)),
  OnLoad = not(proplists:get_bool(no_onload,Options)),
  ?debug(Module),
  ?debug(Properties),
  ?debug(Options),
  ?debug(Sources),
  ?debug(GenCode),
  ?debug(OnLoad),
  Forms = code_gen(Module,Properties,Sources,Sources,GenCode,OnLoad),
  ?debug("Forms = ~s",[erl_prettypr:format(?forms(Forms))]),
  Forms.

reload(Module,Properties,Sources) ->
  Forms = code_gen(Module,Properties,load(Sources),Sources,true,false),
  ?debug("Forms = ~s",[erl_prettypr:format(?forms(Forms))]),
  File =
    case code:which(Module) of
      non_existing -> ?MODULE_STRING++":"++atom_to_list(Module);
      Path -> Path
    end,
  case ?debug(compile:forms(Forms,[return])) of
    {ok,Module,Beam} ->
      % already loaded...
      case ?debug(code:load_binary(Module,File,Beam)) of
        {module,Module} -> ok;
        Error -> Error
      end;
    {ok,Module,Beam,[]} ->
      % already loaded...
      case ?debug(code:load_binary(Module,File,Beam)) of
        {module,Module} -> ok;
        Error -> Error
      end;
    Error -> Error
  end.

% log_msgs(Module,{Line,Mod,Err}) ->
%   io:format("~p(~p) ~s",[Module,Line,Mod:format_error(Err)]);
% log_msgs(Module,{Errs,Warns}) ->
%   log_msgs(Module,Errs),
%   log_msgs(Module,Warns);
% log_msgs(_Module,[]) -> ok;
% log_msgs(Module,[E|Es]) ->
%   log_msgs(Module,E),
%   log_msgs(Module,Es).

load(Sources) ->
  [ case Source of
      {application,Application} -> {proplists,application:get_all_env(Application)};
      {file,File} ->
        case file:consult(File) of
          {ok,Props} -> {proplists,Props};
          {error,_} -> {proplists,[]}
        end;
      {proplists,Props} -> {proplists,Props};
      _ -> {proplists,[]}
    end || Source <- Sources ].
  
  
  
code_gen(Module,Properties,Sources,OrigSources,GenCode,OnLoad) ->
  % io:format("~p~n",[catch erlang:error(debug,[Module,Properties,Sources,GenCode,OnLoad])]),
  NeedsGen = lists:any(fun ({application,_}) -> true; ({file,_}) -> true; (_) -> false end,OrigSources),
  ?debug(Module),
  ?debug(Properties),
  ?debug(Sources),
  ?debug(OrigSources),
  ?debug(GenCode),
  ?debug(OnLoad),
  ?debug(NeedsGen),
  erl_syntax:revert_forms(?forms(
  [ ?attribute(module,Module) ]
  ++
  [ ?export(Prop,0) || {Prop,_} <- Properties ]
  ++
  case NeedsGen andalso GenCode of
    true ->
      [ ?export(reload,0) ]
      ++
      case OnLoad of
        true -> [?attribute(on_load,{'reload#',0})];
        _ -> []
      end;
    _ -> []
  end
  ++
  [ ?export(get,0), ?export(get,1) ]
  ++
  case OrigSources of
    [{application,_}|_] -> [ ?export(set,2), ?export(set,1) ];
    _ -> []
  end
  ++
  [ 
      gen_prop(Prop,Sources++[{proplists,Properties}])
  || {Prop,_} <- Properties ]
  ++
  case NeedsGen andalso GenCode of
    true ->
      [gen_reload(Module,Properties,OrigSources)]
      ++
      case OnLoad of
        true -> [?function('reload#',[?clause([],none,[?apply(spawn,[?func(reload,0)]),?atom(ok)])])];      
        _ -> []
      end;
    _ -> []
  end
  ++
  [ gen_get(Properties)
  , gen_get_all(Properties)
  ]
  ++
  case OrigSources of
    [{application,Application}|_] ->
      [ gen_set()
      , gen_set_all(Application,NeedsGen andalso GenCode)
      ];
    _ -> []
  end)).


gen_prop(Prop,Sources) -> 
  ?function(Prop,[?clause([],none,[gen_prop_value(Prop,Sources)])]).
gen_prop_value(_Prop,[]) -> ?atom(undefined);
gen_prop_value(Prop,[{proplists,PropList}|Sources]) ->
  case proplists:lookup(Prop,PropList) of
    {Prop,Val} -> ?abstract(Val);
    none -> gen_prop_value(Prop,Sources)
  end;
gen_prop_value(Prop,[{application,Application}|Sources]) ->
  ?cases(?apply(application,get_env,[?atom(Application),?atom(Prop)]),
    [ ?clause([?tuple([?atom(ok),?var('Value')])],none,[?var('Value')])
    , ?clause([?atom(undefined)],none,[gen_prop_value(Prop,Sources)])
    ]);
gen_prop_value(Prop,[{file,File}|Sources]) ->
  Var = ?var("Prop"++integer_to_list(length(Sources))),
  ?cases(
    ?cases(?apply(file,consult,[?string(File)]),
      [ ?clause([?tuple([?atom(ok),Var])],none,[?apply(proplists,lookup,[?atom(Prop),Var])])
      , ?clause([?tuple([?atom(error),?underscore])],none,[?atom(none)])
      ]),
    [ ?clause([?tuple([?atom(Prop),?var('Value')])],none,[?var('Value')])
    , ?clause([?atom('none')],none,[gen_prop_value(Prop,Sources)])
    ]);
gen_prop_value(Prop,[_|Sources]) -> gen_prop_value(Prop,Sources).

gen_reload(Module,Properties,Sources) ->
  ?function(reload,
    [ ?clause([],none,[?apply(properties_pt,reload,[?atom(Module),?abstract(Properties),?abstract(Sources)])]) ]).

gen_get(Properties) ->
  ?function(get,
      [ ?clause([?atom(Prop)],none,[?apply(Prop,[])]) || {Prop,_} <- Properties ]
      ++
      [ ?clause([?underscore],none,[?atom(undefined)]) ]
    ).
gen_get_all(Properties) ->
  ?function(get,
      [ ?clause([],none,[?list([ ?tuple([?atom(Prop),?apply(Prop,[])]) || {Prop,_} <- Properties ])]) ]
    ).

gen_set() ->
  ?function(set,[?clause([?var('Key'),?var('Value')],none,[?apply(set,[?list([?tuple([?var('Key'),?var('Value')])])])])]).
gen_set_all(Application,Reload) ->
  ?function(set,
    [ ?clause([?nil],none,
        case Reload of
          true -> [?apply(reload,[])];
          _ -> [?atom(ok)]
        end)
    , ?clause([?list([?tuple([?var('K'),?var('V')])],?var('Opts'))],none,
        [ ?apply(application,set_env,[?atom(Application),?var('K'),?var('V')])
        , ?apply(set,[?var('Opts')])
        ])
    , ?clause([?var('Opt')],none,[?apply(set,[?list([?var('Opt')])])])
    ]).

getModule(Forms) ->
  case erl_syntax_lib:fold(fun (Form,Accum) ->
        case erl_syntax:type(Form) of
          attribute ->
            case erl_syntax_lib:analyze_attribute(Form) of
              {module,Module} -> Accum ++ [ Module ];
              _ -> Accum
            end;
          _ -> Accum
        end
    end,[],Forms) of
      [Module] -> Module
  end.

getProperties(Forms) ->
  erl_syntax_lib:fold(fun (Form,Accum) ->
        case erl_syntax:type(Form) of
          attribute ->
            case erl_syntax_lib:analyze_attribute(Form) of
              {property,{property,Prop}} -> Accum ++ [ property(Prop) ];
              _ -> Accum
            end;
          _ -> Accum
        end
    end,[],Forms).

getSources(Forms) ->
  erl_syntax_lib:fold(fun (Form,Accum) ->
        case erl_syntax:type(Form) of
          attribute ->
            case erl_syntax_lib:analyze_attribute(Form) of
              {source,{source,Source}} -> Accum ++ [ Source ];
              _ -> Accum
            end;
          _ -> Accum
        end
    end,[],Forms).

property(Prop) when is_atom(Prop) -> {Prop,undefined};
property(Props) when is_list(Props) -> [ property(P) || P<-Props ];
property(Prop) -> Prop.
  

