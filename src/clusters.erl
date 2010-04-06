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

-import(lists, [sum/1,foldl/3,sort/1,reverse/1]).
-import(math,  [pow/2,sqrt/1]).


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
    Sum2Sq = sum([pow(S2,2) || S2 <- V1]),

    % Sum up the products
    Psum = sum([S1*S2 || {S1,S2} <- lists:zip(V1,V2)]),
    
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
%% where: datarows() ::= {rowdata::list(), ... }
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
     list_to_tuple(Data)}.
