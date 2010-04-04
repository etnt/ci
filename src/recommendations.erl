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
%%      
%% Return a guess at what my rating would be.
%% Produce a weighted score that calculates how similar
%% a critics is to me and what score the movie got from him/her. 
%% Try:
%%
%%   get_recommendations("Toby")
%%
%% By swapping the data you can also get a recommended critics for a movie:     
%%
%%  get_recommendations("Just My Luck",sim_pearson,transform_data).
%%
get_recommendations(Person) ->
    get_recommendations(Person, sim_pearson, data).

get_recommendations(Person, Similarity, Prefs) ->
    Dict = sim_sum(Person, Similarity, Prefs, orddict:new()),
    reverse(sort([{Total/SimSum, Name} || 
                     {Name, {Total,SimSum,_N}} <- orddict:to_list(Dict)])).
    

sim_sum(Person, Similarity, Prefs, Dict) ->
    F = fun({Other,L},Acc) when Other /= Person -> 
                sim_sum_prefs(L, ?MODULE:Similarity(Person,Other,Prefs), Acc);
           (_,Acc) -> Acc
        end,
    foldl(F, Dict, ?MODULE:Prefs()).

sim_sum_prefs(Prefs, Sim, Dict) when Sim > 0 ->
    F = fun({Movie,Point}, Acc) -> sim_sum_update(Movie, Point, Sim, Acc) end,
    foldl(F, Dict, Prefs);
%% Ignore Scores of zero or lower
sim_sum_prefs(_Prefs, _Sim, Dict) ->
    Dict.

sim_sum_update(Key, Point, Sim, Dict) ->
    orddict:update(Key, 
                   fun({V,S,N}) -> {(Point*Sim)+V,S+Sim,N+1} end, 
                   {Point*Sim,Sim,1}, 
                   Dict).
           

%% @doc Critcs with most similar tastes as 'mine'
%%
%% Which advice should I take?
%% Try: 
%%
%%   top_matches("Toby",3) to find out.
%%
%% Also, by swapping the data, you'll find out similar movies, try:
%%
%%   top_matches("Superman Returns",3,sim_pearson,transform_data).
%%
top_matches(P)              -> top_matches(P,5).
top_matches(P,N)            -> top_matches(P,N,sim_pearson).
top_matches(P,N,Similarity) -> top_matches(P,N,Similarity,data).
top_matches(Person,N,Similarity,Prefs) ->
    Scores = [{?MODULE:Similarity(Person,Other,Prefs),Other}
              || {Other,_} <- ?MODULE:Prefs(),
                 Other /= Person],
    take(N,reverse(sort(Scores))).
    


%% @doc Pearson Correlation Score
%%      
%% This method tend to give better results when the
%% input data isn't well normalized.
%% 
sim_pearson(P1,P2) ->
    sim_pearson(P1,P2,data).

sim_pearson(P1,P2,Prefs) ->
    % Add up all the preferences
    {Sum1,Sum2} = sum_pearson(P1,P2,Prefs,
                              fun(S1,S2) -> {S1,S2} end),

    % Sum up the squares
    {Sum1Sq,Sum2Sq} = sum_pearson(P1,P2,Prefs,
                                  fun(S1,S2) -> {pow(S1,2),pow(S2,2)} end),

    % Sum up the products
    {Psum,_} = sum_pearson(P1,P2,Prefs,
                           fun(S1,S2) -> {S1*S2,0} end),
    
    % Calculate Pearson score
    N   = sum(process_common_prefs(P1,P2,Prefs,
                                   fun(_,_) -> 1 end)),
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
    sum_pearson(P1,P2,data,F).

sum_pearson(P1,P2,Prefs,F) ->
    L = process_common_prefs(P1,P2,Prefs,F),
    foldl(fun({X1,X2}, {Acc1,Acc2}) -> {X1+Acc1,X2+Acc2} end, {0,0}, L).


%% @doc Euclidean Distance
%%      
%% Computes a similarity score between two persons by 
%% calculating the distance between each preference.
%% 
sim_distance(P1,P2) ->
    sim_distance(P1,P2,data).

sim_distance(P1,P2,Prefs) ->
    1/(1+sqrt(sum(sim_distances(P1,P2,Prefs)))).

sim_distances(P1,P2,Prefs) ->
    process_common_prefs(P1,P2,Prefs,fun(S1,S2) -> pow(S1-S2,2) end).



process_common_prefs(P1,P2,Prefs,F) ->
    [F(S1,S2) || {N1,S1} <- prefs(P1,Prefs),
                 {N2,S2} <- prefs(P2,Prefs),
                 N1 == N2].

%% Group the data around the Movies instead.
transform_data() ->
    orddict:to_list(
      lists:foldl(
        fun({Person,L}, Dict) -> 
                lists:foldl(
                  fun({Movie,Pref},Dict2) ->
                          orddict:update(
                            Movie,
                            fun(Z) -> [{Person,Pref}|Z] end,
                            [{Person,Pref}],
                            Dict2)
                  end, Dict, L)
        end, orddict:new(), data())
     ).
                                                                   
                        
prefs(P,Prefs) ->
    proplists:get_value(P, ?MODULE:Prefs()).

data() ->
    {ok,Data} = file:consult("priv/recommendations.data"),
    Data.
    
%% Should be in lists.erl...
take(N,L) ->
    {R,_} = lists:split(N,L),
    R.
    


            
                            
