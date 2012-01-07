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
  PropertyDefaults = getProperties(Forms1),
  Properties = [P || {P,_} <- PropertyDefaults],
  Options = lists:flatten([ Opt ||  {?MODULE,Opt} <- proplists:get_all_values(?MODULE,COptions) ]),
  Sources0 = static(getSources(Forms1)),
  GenCode = not(proplists:get_bool(no_codegen,Options)),
  OnLoad = not(proplists:get_bool(no_onload,Options)),
  Static = proplists:get_bool(static,Options),
  Sources = simplify(case Static of
    true -> Sources0 ++ load(Sources0) ++ [{proplists,PropertyDefaults}];
    _ -> Sources0 ++ [{proplists,PropertyDefaults}]
  end,Properties), 
  ?debug(Module),
  ?debug(Properties),
  ?debug(PropertyDefaults),
  ?debug(Options),
  ?debug(Sources0),
  ?debug(Sources),
  ?debug(GenCode),
  ?debug(OnLoad),
  ?debug(Static),
  Forms = code_gen(Module,Properties,case Static of true -> load(Sources); _ -> Sources end,Sources,GenCode,OnLoad),
  ?debug("Forms = ~s",[erl_prettypr:format(?forms(Forms))]),
  Forms.

simplify(Props,Keys) -> simplify(Props,[],Keys).
simplify([],_Keys,_Only) -> [];
simplify([{proplists,P1},{proplists,P2}|Ps],Keys,Only) ->
  simplify([{proplists,P1++P2}|Ps],Keys,Only);
simplify([{proplists,P1}|Ps],Keys1,Only) ->
  case simplify2(P1,Keys1,Only,[]) of
    {[],Keys2} -> simplify(Ps,Keys2,Only);
    {P2,Keys2} -> [{proplists,P2}|simplify(P2,Keys2,Only)]
  end;
simplify([P|Ps],Keys,Only) -> [P|simplify(Ps,Keys,Only)].
simplify2([P|Ps],Keys,Only,Accum) ->
  {K,V} = proplists:property(P),
  case {is_in(K,Keys),is_in(K,Only)} of
    {false,true} -> simplify2(Ps,[K|Keys],Only,[{K,V}|Accum]);
    _ -> simplify2(Ps,Keys,Only,Accum)
  end;
simplify2([],Keys,_Only,Accum) -> {Accum,Keys}.

is_in(_K,[]) -> false;
is_in(K,[K|_]) -> true;
is_in(K,[_|L]) -> is_in(K,L).
  
        

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

static([]) -> [];
static([A|B]) -> [static(A)|static(B)];
static({static,A}) -> load(A);
static(A) -> A.

load([]) -> [];
load([A|B]) -> [load(A)|load(B)];
load({static,A}) -> load(A);
load({application,A}) -> {proplists,application:get_all_env(A)};
load({file,A}) -> 
  case file:consult(A) of
    {ok,Props} -> {proplists,Props};
    {error,_} -> {proplists,[]}
  end;
load(A) -> A.

-compile({nowarn_unused_function,[value/2]}).
value(_Key,[]) -> undefined;
value(Key,[A|B]) ->
  case value(Key,A) of
    undefined -> value(Key,B);
    Val -> Val
  end;
value(Key,{static,A}) -> value(Key,A);
value(Key,{application,A}) ->
  case application:get_env(A,Key) of
    {ok,Val} -> Val;
    _ -> undefined
  end;
value(Key,{file,A}) ->
  case file:consult(A) of
    {ok,P} -> proplists:get_value(Key,P);
    _ -> undefined
  end;
value(Key,{proplists,A}) ->
  proplists:get_value(Key,A);
value(_Key,_Unknown) -> undefined.

value_code(Key,Source) ->
  case value_code_(Key,Source) of
    {Code,_Res} -> Code
  end.

value_code_(_Key,[]) -> {?atom(undefined),novalue};
value_code_(Key,[A]) -> value_code(Key,A);
value_code_(Key,[A|B]) ->
  case value_code(Key,A) of
    {Code,value} -> {Code,value};
    {ACode,ARes} -> 
      case value_code_(Key,B) of
        {_,novalue} -> {ACode,ARes};
        {BCode,BRes} ->
          Var = var(),
          {?cases(ACode,[?clause([?atom(undefined)],none,[BCode]),?clause([Var],none,[Var])]),BRes}
      end
  end;
value_code_(Key,{static,A}) -> value_code_(Key,load(A));
value_code_(Key,{application,A}) ->
  {?apply(application,get_env,[?atom(A),?atom(Key)]),undefined};
value_code_(Key,{file,A}) ->
  Var = var(),
  {?cases(?apply(file,consult,[?string(A)]),
    [ ?clause([?tuple([?atom(ok),Var])],none,[?apply(proplists,get_value,[?atom(Key),Var])])
    , ?clause([?underscore],none,[?atom(undefined)])
    ]),undefined};
value_code_(Key,{proplists,A}) ->
  case proplists:get_value(Key,A) of
    undefined -> {?atom(undefined),novalue};
    V -> {?abstract(V),value}
  end;
value_code_(_Key,_Unknown) ->
  {?atom(undefined),novalue}.
  


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
      gen_prop(Prop,Sources)
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
  ?function(Prop,[?clause([],none,[value_code(Prop,Sources)])]).

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
              {source,{source,Source}} -> Accum ++ lists:flatten([Source]);
              _ -> Accum
            end;
          _ -> Accum
        end
    end,[],Forms).

property(Prop) when is_atom(Prop) -> {Prop,undefined};
property(Props) when is_list(Props) -> [ property(P) || P<-Props ];
property(Prop) -> Prop.
  

