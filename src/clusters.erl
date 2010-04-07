%%%-------------------------------------------------------------------
%%% Created :  1 Apr 2010 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%%
%%% @doc Examples from the O'Reilly book: "Collective Intelligence"
%%%
%%%-------------------------------------------------------------------
-module(clusters).
%-export([sim_distance/2]).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-import(lists, [sum/1,foldl/3,sort/1,reverse/1,zip/2,seq/2]).
-import(math,  [pow/2,sqrt/1]).

-define(p(Fs,As), true).
%-define(p(Fs,As), io:format("~p: "++Fs,[time()|As])).

%% @doc Output the data from one cluster
%%
%%  {RowNames,ColNames,Data} = clusters:csv(Fname).
%%  C=clusters:kcluster(Data,10).
%%  clusters:output(Fname2, RowNames, ColNames, Data, array:get(0,C)).
%%
output(Fname, RowNames, ColNames, Data, Indices) ->
    DataTuple = list_to_tuple(Data),
    Cs = string:join(tuple_to_list(ColNames),","),
    L = string:join([Cs | [string:join([e(I,RowNames)
                                        |[float_to_str(F) 
                                          || F <- e(I,DataTuple)]],",") 
                     || I <- Indices]],"\n"),
    file:write_file(Fname,list_to_binary(L)).
    
float_to_str(F) when is_float(F) -> io_lib:format("~.2f",[F]);
float_to_str(X)                  -> io_lib:format("~w",[X]).

%% @doc Print what people want in a cluster
%% Try: 
%%
%%  {RowNames,ColNames,Data} = clusters:zebodata().
%%  C=clusters:kcluster(Data,10).
%%  C=clusters:kcluster(Data,10, manhattan). % alt. tanimoto works bad, why?
%%  clusters:wants(RowNames, array:get(0,C)).
%%  [clusters:wants(RowNames, array:get(J,C)) || J <- lists:seq(0,9)].
%%
wants(RowNames,Indices) ->
    [e(I,RowNames) || I <- Indices].
    
%% @doc Print the blog names of a cluster
%% Try: 
%%
%%  {RowNames,ColNames,Data} = clusters:blogdata().
%%  C=clusters:kcluster(Data,10).
%%  clusters:blognames(RowNames, array:get(0,C)).
%%  [clusters:blognames(RowNames, array:get(J,C)) || J <- lists:seq(0,9)].
%%
blognames(RowNames,Indices) ->
    [e(I,RowNames) || I <- Indices].
    

%% @doc K-means clustering
%%
%% Begin with K randomly placed Centroids (points in space that represent
%% the center of a cluster). Assign every item to the nearest centroid.
%% Move the centroid to the average position if its cluster. Repeat until
%% no more changes occurs.
%%
kcluster(Rows)                   -> kcluster(Rows, 4).
kcluster(Rows, K)                -> kcluster(Rows, K, pearson).
kcluster(Rows, K, Distance)      -> kcluster(Rows, K, Distance, 100).
kcluster(Rows, K, Distance, Max) ->
    RowsTuple = list_to_tuple(Rows),

    ?p("Determine the minimum and maximum values for each column.~n",[]),
    Columns = rows2columns(Rows),
    ColRanges = [min_max(Column) || Column <- Columns],

    ?p("Create k randomly placed centroids.~n",[]),
    Clusters = mk_random_clusters(K, ColRanges, Distance),

    try 
        ?p("Begin iterate over the positions of the centroids.~n",[]),
        e(2,foldl(fun(_,{Cs,Matches,Iter}) ->
                          io:format("Iter ~p/~p~n",[Iter,Max]),
                          case bestmatches(Rows, Distance, K, Cs) of
                              Matches     -> throw({finished,Matches});
                              BestMatches -> 
                                  {move_centroids(RowsTuple,K,BestMatches),
                                   BestMatches, Iter+1}
                          end
                  end, {Clusters,mk_array(K),1}, seq(1,Max)))
    catch 
        throw:{finished,Ms} -> Ms 
    end.

mk_random_clusters(K, ColRanges, tanimoto) ->
    random:seed(erlang:now()),
    [[random:uniform() || _ <- ColRanges] 
     || _J <- seq(1,K)];
mk_random_clusters(K, ColRanges, _) ->
    random:seed(erlang:now()),
    [[random:uniform()*(Max-Min) + Min
      || {Min,Max} <- ColRanges]
     || _J <- seq(1,K)].
                                  

%% @doc Move the centroids to the average of their members,
%%
move_centroids(RowsTuple,K,Bestmatches) ->
    ?p("Move the centroids to the average of their members.~n",[]),
    [mk_new_centroid(RowsTuple,Bestmatches,J) || J <- seq(0,K-1)].

mk_new_centroid(RowsTuple, Bestmatches, J) ->
    try
        C = [average(Column) 
             || Column <- rows2columns([e(I,RowsTuple) 
                                        || I <- array:get(J,Bestmatches)])],
        % assert
        true = length(C) > 0,
        C
    catch _:_ ->
            % No Item was attached to this cluster.
            % Let's just make Zero centroid.
            [0.0 || _ <- seq(1,length(e(1,RowsTuple)))]
    end.
            

average(L)             -> average(L,0,0).
average([H|T],Sum,Len) -> average(T,H+Sum,1+Len);
average([],Sum,Len)    -> Sum/Len.
    

%% @doc Group the rows around the closest centroid.
%% Return an array of size K, where each entry I contains a list
%% of the rows being closest to the I:th centroid.
%%
bestmatches(Rows, Distance, K, Clusters) ->
    ?p("Group the rows around their closest centroid.~n",[]),
    e(2, foldl(fun(Row,{J,Arr}) -> 
                       I = bestmatch(Clusters, Row, Distance),
                       {J+1,append_array_val(I,J,Arr)}
               end, {1, mk_array(K)}, Rows)).


%% @doc Transpose a list of rows to a list of columns.
%% In:  [[C1,C2,...Cn] = R1, R2,...Rn] = Rows
%% Out: [[R1C1,R2C1,...RnC1],...[R1Cn,R2Cn,...RmCn]] = Columns
%%
rows2columns([])                      -> [];
rows2columns(Rows) when is_list(Rows) ->
    ?p("Transpose a list of rows to a list of columns.~n",[]),
    [[e(I,RowTuple) || RowTuple <- [list_to_tuple(Row) || Row <- Rows]]
     || I <- seq(1,length(hd(Rows)))].

mk_array(K) ->    
    array:new([{size,K},{fixed,true},{default,[]}]).

append_array_val(K,V,Arr) ->
    array:set(K,[V|array:get(K, Arr)],Arr).


%% @doc Return which centroid is the closest to the row
%%
bestmatch(Clusters, Row, Distance) ->
    R = foldl(fun(Crow, {J,Bestmatch,Bdist}) ->
                      D = ?MODULE:Distance(Crow,Row),
                      if (D<Bdist) -> {J+1, J+1, D};
                         true   -> {J+1, Bestmatch, Bdist}
                      end
              end, {0, 0, ?MODULE:Distance(hd(Clusters),Row)}, tl(Clusters)),
    e(2,R).
                          
min_max([H|T]) -> min_max(T,H,H).

min_max([H|T],Min,Max) when H=<Min -> min_max(T,H,Max);
min_max([H|T],Min,Max) when H>Max  -> min_max(T,Min,H);
min_max([_|T],Min,Max)             -> min_max(T,Min,Max);
min_max([],Min,Max)                -> {Min,Max}.
    
e(I,T) when is_integer(I), is_tuple(T) ->
    element(I,T).


%% @doc Tanimoto coefficient
%%      
%% For datasets that has just 1s and 0s (for presence or absence)
%% we measure the overlap, i.e the ratio of the intersection set.
%% A returned value of 1.0 indicates no sharing where 0.0 means
%% that the two sets are exactly the same.
%% 
tanimoto(V1,V2) ->
    {C1,C2,Shr} = 
        foldl(fun({E1,E2},{S1,S2,S}) -> {S1+z(E1),S2+z(E1),S+z(E1,E2)}
               end, {0,0,0}, lists:zip(V1,V2)),
    1.0-(1.0*Shr)/(C1+C2-Shr).

z(0) -> 0;
z(_) -> 1.

z(X,X) -> 1;
z(_,_) -> 0.

%% @doc Manhattan distance
%%      
%% The sum of the lengths of the projections of 
%% the line segment between the points.
%% 
manhattan(V1,V2) ->
    sqrt(sum([pow(X1-X2,2) || {X1,X2} <- lists:zip(V1,V2)])/length(V1)).
    

%% @doc Pearson Correlation Score
%%      
%% Takes two list of numbers and return their correlation score.
%% Nb: smaller == more similar
%% 
pearson(V1,V2) ->
    % Simple sums
    Sum1 = sum(V1),
    Sum2 = sum(V2),

    % Sum of the squares
    Sum1Sq = sum([pow(S1,2) || S1 <- V1]),
    Sum2Sq = sum([pow(S2,2) || S2 <- V2]),

    % Sum up the products
    Psum = sum([S1*S2 || {S1,S2} <- zip(V1,V2)]),
    
    % Calculate r (Pearson score)
    N   = length(V1),
    Num = Psum - (Sum1*Sum2/N),
    Den = sqrt( (Sum1Sq - pow(Sum1,2)/N) * (Sum2Sq - pow(Sum2,2)/N) ),

    % Return a smaller distance between items that are more similar.
    if (Den == 0) -> 0;
       true       -> 1.0 - Num/Den
    end.

-ifdef(EUNIT).
pearson_test() ->
    {_RowNames,_ColNames,Data} = clusters:blogdata(),
    V1 = element(1, Data),
    V2 = element(2, Data),
    ?assertMatch(0.9853239168490683, clusters:pearson(V1, V2)).
-endif.


%% @doc Read the blogdata, taken from http://kiwitobes.com/clusters/blogdata.txt
%%
%% Return: {RowNames::tuple(), ColNames::tuple(), RowData::datarows()}
%% where: datarows() ::= [rowdata::list(), ... ]
%%
blogdata() ->
    {ok, Bin} = ci:get_blogdata(),
    data(Bin).

%% @doc Read the Zebo data, taken from http://kiwitobes.com/clusters/zebo.txt
%%
%% Return: {RowNames::tuple(), ColNames::tuple(), RowData::datarows()}
%% where: datarows() ::= [rowdata::list(), ... ]
%%
zebodata() ->
    {ok, Bin} = ci:get_zebo(),
    data(Bin).

%% @doc Read a csv file containing data
%%
csv(Fname) ->
    {ok,Bin} = file:read_file(Fname),
    data([comma2tab(Char) || Char <- binary_to_list(Bin)]).

comma2tab($,) -> $\t;
comma2tab(C)  -> C.
    

data(Bin) when is_binary(Bin) -> data(binary_to_list(Bin));
data(List) when is_list(List) ->
    AllLines  = string:tokens(List, "\r\n"),
    
    % First line is the column titles
    [ColNames|Lines] = AllLines,
    {RowNames, Data} = 
        foldl(fun(Line, {RowNames_,Data_}) ->
                      % First column in each row is the rowname
                      [Row|Ds] = string:tokens(Line, "\t\r"),
                      {[Row|RowNames_], 
                       % The data is a list of floats
                       [[list_to_integer(massage(string:strip(D)))*1.0 
                         || D <- Ds]|Data_]}
              end, {[],[]}, Lines),
    % Return
    {list_to_tuple(RowNames), 
     list_to_tuple(string:tokens(ColNames,"\t")), 
     Data}.

%% Do special massage on some values.
massage("true")         -> "1";
massage("false")        -> "0";
massage("undefined")    -> "0";
massage("none")         -> "0";
massage("unmarried")    -> "1";
massage("married")      -> "2";
massage("divorced")     -> "3";
massage("ex_married")   -> "4";
massage("partnership")  -> "5";
massage("widowed")      -> "6";
massage(Else)           -> Else.

    
