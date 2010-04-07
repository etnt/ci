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
kcluster(Rows)              -> kcluster(Rows, 4).
kcluster(Rows, K)           -> kcluster(Rows, K, pearson).
kcluster(Rows, K, Distance) ->
    RowsTuple = list_to_tuple(Rows),

    ?p("Determine the minimum and maximum values for each column.~n",[]),
    Columns = rows2columns(Rows),
    ColRanges = [min_max(Column) || Column <- Columns],

    ?p("Create k randomly placed centroids.~n",[]),
    random:seed(erlang:now()),
    Clusters = [[random:uniform()*(Max-Min) + Min
                  || {Min,Max} <- ColRanges]
                || _J <- seq(1,K)],

    try 
        ?p("Begin iterate over the positions of the centroids.~n",[]),
        e(2,foldl(fun(_,{Cs,Matches}) ->
                          case bestmatches(Rows, Distance, K, Cs) of
                              Matches     -> throw({finished,Matches});
                              BestMatches -> 
                                  {move_centroids(RowsTuple,K,BestMatches),
                                   BestMatches}
                          end
                  end, {Clusters,mk_array(K)}, seq(1,100)))
    catch 
        throw:{finished,Ms} -> Ms 
    end.
                                  

%% @doc Move the centroids to the average of their members,
%%
move_centroids(RowsTuple,K, Bestmatches) ->
    ?p("Move the centroids to the average of their members.~n",[]),
    [[average(Column) 
      || Column <- rows2columns([e(I,RowsTuple) 
                                 || I <- array:get(J,Bestmatches)])]
     || J <- seq(0,K-1)].

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
    AllLines  = string:tokens(binary_to_list(Bin), "\r\n"),
    
    % First line is the column titles
    [ColNames|Lines] = AllLines,
    {RowNames, Data} = 
        foldl(fun(Line, {RowNames_,Data_}) ->
                      % First column in each row is the rowname
                      [Row|Ds] = string:tokens(Line, "\t\r"),
                      {[Row|RowNames_], 
                       % The data is a list of floats
                       [[list_to_integer(D)*1.0 || D <- Ds]|Data_]}
              end, {[],[]}, Lines),
    % Return
    {list_to_tuple(RowNames), 
     list_to_tuple(string:tokens(ColNames,"\t")), 
     Data}.
