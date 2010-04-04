%%%-------------------------------------------------------------------
%%% Created :  1 Apr 2010 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%%
%%% @doc Examples from the O'Reilly book: "Collective Intelligence"
%%%
%%%-------------------------------------------------------------------
-module(recommendations).
%-export([sim_distance/2]).
-compile(export_all).

-import(lists, [sum/1,foldl/3,sort/1,reverse/1]).
-import(math,  [pow/2,sqrt/1]).


%% @doc Get a movie recommendation.
%%      Also, return a guess at what my rating would be.
%%      Produce a weighted score that calculates how similar
%%      a critics is to me and what score the movie got from him/her. 
%%
get_recommendations(Person) ->
    get_recommendations(Person, sim_pearson).

get_recommendations(Person, Similarity) ->
    Dict = sim_sum(Person, Similarity, orddict:new()),
    reverse(sort([{Total/SimSum, Name} || 
                     {Name, {Total,SimSum,_N}} <- orddict:to_list(Dict)])).
    

sim_sum(Person, Similarity, Dict) ->
    F = fun({Other,Prefs},Acc) when Other /= Person -> 
                sim_sum_prefs(Prefs, ?MODULE:Similarity(Person,Other), Acc);
           (_,Acc) -> Acc
        end,
    foldl(F, Dict, data()).

sim_sum_prefs(Prefs, Sim, Dict) when Sim > 0 ->
    F = fun({Movie,Point}, Acc) -> sim_sum_update(Movie, Point, Sim, Acc) end,
    foldl(F, Dict, Prefs);
%% Ignore Scores of zero or lower
sim_sum_prefs(_Prefs, _Sim, Dict) ->
    Dict.

sim_sum_update(Key, Point, Sim, Dict) ->
    try orddict:update(Key, fun({V,S,N}) -> {(Point*Sim)+V,S+Sim,N+1} end, Dict)
    catch _:_ -> orddict:store(Key, {Point*Sim,Sim,1}, Dict) end.
           

%% @doc Critcs with most similar tastes as 'mine'
%%
%% Which advice should I take?
%% Try: top_matches("Toby",3) to find out.
%%
top_matches(P)   -> top_matches(P,5).
top_matches(P,N) -> top_matches(P,N,sim_pearson).
top_matches(Person,N,Similarity) ->
    Scores = [{?MODULE:Similarity(Person,Other),Other}
              || {Other,_} <- data(),
                 Other /= Person],
    take(N,reverse(sort(Scores))).
    


%% @doc Pearson Correlation Score
%%      
%% This method tend to give better results when the
%% input data isn't well normalized.
%% 
sim_pearson(P1,P2) ->
    % Add up all the preferences
    {Sum1,Sum2} = sum_pearson(P1,P2,fun(S1,S2) -> {S1,S2} end),

    % Sum up the squares
    {Sum1Sq,Sum2Sq} = sum_pearson(P1,P2,fun(S1,S2) -> {pow(S1,2),pow(S2,2)} end),

    % Sum up the products
    {Psum,_} = sum_pearson(P1,P2,fun(S1,S2) -> {S1*S2,0} end),
    
    % Calculate Pearson score
    N   = sum(process_common_prefs(P1,P2,fun(_,_) -> 1 end)),
    Num = Psum - (Sum1*(Sum2/N)),
    Den = den(Sum1Sq,Sum2Sq,Sum1,Sum2,N),
    if (Den == 0) -> 
            0;
       true ->
            Num/Den
    end.

den(Sum1Sq,Sum2Sq,Sum1,Sum2,N) ->
    sqrt(Sum1Sq - pow(Sum1,2) / N) * sqrt(Sum2Sq - pow(Sum2,2) / N).
                                   
sum_pearson(P1,P2,F) ->
    L = process_common_prefs(P1,P2,F),
    foldl(fun({X1,X2}, {Acc1,Acc2}) -> {X1+Acc1,X2+Acc2} end, {0,0}, L).


%% @doc Euclidean Distance
%%      
%% Computes a similarity score between two persons by 
%% calculating the distance between each preference.
%% 
sim_distance(P1,P2) ->
    1/(1+sqrt(sum(sim_distances(P1,P2)))).

sim_distances(P1,P2) ->
    process_common_prefs(P1,P2,fun(S1,S2) -> pow(S1-S2,2) end).



process_common_prefs(P1,P2,F) ->
    [F(S1,S2) || {N1,S1} <- prefs(P1),
                 {N2,S2} <- prefs(P2),
                 N1 == N2].

prefs(P) ->
    proplists:get_value(P, data()).

data() ->
    {ok,Data} = file:consult("priv/recommendations.data"),
    Data.
    
%% Should be in lists.erl...
take(N,L) ->
    {R,_} = lists:split(N,L),
    R.
    


            
                            
