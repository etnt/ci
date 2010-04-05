%%%-------------------------------------------------------------------
%%% Created :  1 Apr 2010 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%%
%%% @doc Examples from the O'Reilly book: "Collective Intelligence"
%%%
%%%-------------------------------------------------------------------
-module(recommendations).
%-export([sim_distance/2]).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-import(lists, [sum/1,foldl/3,sort/1,reverse/1]).
-import(math,  [pow/2,sqrt/1]).


%% @doc Load the MovieLens dataset.
%%
%% This function creates a dictionary based on the "Movie Lens Data Sets",
%% that can be downloaded from: http://www.grouplens.org/ 
%% Try, for userid(87):
%%
%%  Prefs = load_movie_lens().
%%
%%  % On my Dell Vostro Laptop...
%%  % ...this takes roughly 24 seconds
%%  get_recommendations("87",sim_distance,Prefs).
%%
%%  % This takes about an hour...
%%  ItemSim = calc_similar_items(Prefs,50).
%%
%%  % ...but this only takes ~0.5 seconds.
%%  get_recommended_items(Prefs, ItemSim, "87")
%%
%%  % Use your own movie ratings to get a recommendation
%%  get_recommended_items(ItemSim, [{"GoldenEye (1995)",2.5},...])
%%
load_movie_lens() ->
    %% Get the movie titles.
    {ok,Iz}=file:read_file("priv/u.item.zip"),
    {ok,[{_Fname,IzBin}]} = zip:unzip(Iz,[memory]),
    IzLines = string:tokens(binary_to_list(IzBin), "\n"),
    Movies = foldl(fun(Line,Mtid) -> 
                           [Id,Title|_] = string:tokens(Line,"|"),
                           ets:insert(Mtid,{Id,Title}),
                           Mtid
                   end, ets:new(?MODULE,[]), IzLines),
    
    %% Load data
    {ok,Dz}=file:read_file("priv/u.data.zip"),
    {ok,[{_Fname2,DzBin}]} = zip:unzip(Dz,[memory]),
    DzLines = string:tokens(binary_to_list(DzBin), "\n"),
    foldl(fun(Line,Ptid) -> 
                  [User,Mid,Rating,_Ts|_] = string:tokens(Line,"\t"),
                  ets_append(User,
                             {ets_lookup(Mid,Movies),
                              list_to_integer(Rating)*1.0},
                             Ptid),
                  Ptid
          end, ets:new(?MODULE,[]), DzLines).

append_to_dict(Key, Value, Dict) ->
    try orddict:append(Key, Value, Dict)
    catch _:_ -> orddict:store(Key, [Value], Dict) end.
        


%% @doc Get weighted recommendations using a pre-calculated dictionary.
%%
%% By using a a pre-calculated item similarity dictionary (from 
%% calc_similar_items/N) we don't need to go through the whole dataset.
%% This function will get all items that a user has ranked, find the 
%% similar items, and weight them according to how similar they are.
%%
get_recommended_items(User) ->
    Tid = data2(),
    get_recommended_items(Tid, calc_similar_items(Tid), User).

get_recommended_items(PrefsTid, ItemTid, User) ->
    UserRatings = ets_lookup(User,PrefsTid),
    get_recommended_items(ItemTid, UserRatings).

get_recommended_items(ItemTid, UserRatings) ->
    %% Loop over items rated by this user
    D = foldl(
          fun({Item,Rating}, Dict) ->
                  %% Loop over items similar to this one
                  foldl(
                    fun({Sim,Item2}, Dict2) ->
                            case lists:keymember(Item2, 1, UserRatings) of
                                %% Ignore if this user already has rated this item
                                true  -> Dict2;
                                false -> sim_sum_update(Item2, Rating, Sim, Dict2)
                        end
                    end,
                    Dict, ets_lookup(Item, ItemTid))
          end,
          orddict:new(), UserRatings),
    reverse(sort([{Total/SimSum, Name} || 
                     {Name, {Total,SimSum,_N}} <- orddict:to_list(D)])).
 
    
-ifdef(EUNIT).
get_recommended_items_test() ->
    ?assertMatch({3.1667425234070894,"The Night Listener"},
                 hd(get_recommended_items("Toby"))).
-endif.


%% @doc Create a dictionary of items showing which other item they are most similar to.
%%
calc_similar_items()      -> calc_similar_items(data2()).
calc_similar_items(Tid)   -> calc_similar_items(Tid, 10).
calc_similar_items(Tid,N) -> calc_similar_items(Tid, N, sim_distance).
calc_similar_items(Tid, N, Similarity) ->
    Tid2 = transform_data2(Tid),
    Size = ets:info(Tid2,size),
    R=ets:foldl(
        fun({Item,_},{I,Tid3}) ->
                print(I,Size),
                Scores = top_matches(Item, N, Similarity, Tid2),
                ets:insert(Tid3,{Item,Scores}),
                {I+1,Tid3}
        end, {1,ets:new(?MODULE,[])}, Tid2),
    element(2,R).

-ifdef(EUNIT).
calc_similar_items_test() ->
    ?assertMatch({"Lady in the Water",
                  [{0.4494897427831781,"You, Me and Dupree"},
                   {0.38742588672279304,"The Night Listener"},
                   {0.3483314773547883,"Snakes on a Plane"},
                   {0.3483314773547883,"Just My Luck"},
                   {0.2402530733520421,"Superman Returns"}]},
                 hd(ets:tab2list(calc_similar_items()))).
-endif.

%% Print a status update for large datasets.
print(I,Len) when (I rem 10) == 0 -> io:format("~p/~p~n",[I,Len]);
print(_, _)                         -> ok.


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
    get_recommendations(Person, sim_pearson, data2()).

get_recommendations(Person, Similarity, Tid) ->
    Dict = sim_sum(Person, Similarity, Tid, orddict:new()),
    reverse(sort([{Total/SimSum, Name} || 
                     {Name, {Total,SimSum,_N}} <- orddict:to_list(Dict)])).

-ifdef(EUNIT).
get_recommendations_test() ->
    ?assertMatch({3.9024195568915734,"Superman Returns"}, hd(get_recommendations("Toby"))).
-endif.
    

sim_sum(Person, Similarity, Tid, Dict) ->
    F = fun({Other,L},Acc) when Other /= Person -> 
                sim_sum_prefs(L, ?MODULE:Similarity(Person,Other,Tid), Acc);
           (_,Acc) -> Acc
        end,
    ets:foldl(F, Dict, Tid).

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
%% Also, by swapping the data, you'll find out similar movies, try:<
%%
%%   top_matches("Superman Returns",3,sim_pearson,transform_data).
%%
top_matches(P)              -> top_matches(P,5).
top_matches(P,N)            -> top_matches(P,N,sim_pearson).
top_matches(P,N,Similarity) -> top_matches(P,N,Similarity,data2()).
top_matches(Person,N,Similarity,Tid) ->
    Scores = 
        ets:foldl(
          fun({Other,_},Acc) when Other /= Person ->
                  [{?MODULE:Similarity(Person,Other,Tid),Other}|Acc];
             (_,Acc) -> Acc
          end, [], Tid),
    take(N,reverse(sort(Scores))).

-ifdef(EUNIT).
top_matches_test() ->
    ?assertMatch({0.9912407071619297,"Lisa Rose"}, hd(top_matches("Toby"))).
-endif.
    

%% @doc Pearson Correlation Score
%%      
%% This method tend to give better results when the
%% input data isn't well normalized.
%% 
sim_pearson(P1,P2) ->
    sim_pearson(P1,P2,data2()).

sim_pearson(P1,P2,Tid) ->
    % Add up all the preferences
    {Sum1,Sum2} = sum_pearson(P1,P2,Tid,
                              fun(S1,S2) -> {S1,S2} end),

    % Sum up the squares
    {Sum1Sq,Sum2Sq} = sum_pearson(P1,P2,Tid,
                                  fun(S1,S2) -> {pow(S1,2),pow(S2,2)} end),

    % Sum up the products
    {Psum,_} = sum_pearson(P1,P2,Tid,
                           fun(S1,S2) -> {S1*S2,0} end),
    
    % Calculate Pearson score
    N   = sum(process_common_prefs(P1,P2,Tid,
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
                                   
sum_pearson(P1,P2,Tid,F) ->
    L = process_common_prefs(P1,P2,Tid,F),
    foldl(fun({X1,X2}, {Acc1,Acc2}) -> {X1+Acc1,X2+Acc2} end, {0,0}, L).

-ifdef(EUNIT).
sim_pearson_test() ->
    ?assertMatch(0.9912407071619297, sim_pearson("Toby","Lisa Rose")).
-endif.


%% @doc Euclidean Distance
%%      
%% Computes a similarity score between two persons by 
%% calculating the distance between each preference.
%% 
sim_distance(P1,P2) ->
    sim_distance(P1,P2,data2()).

sim_distance(P1,P2,Tid) ->
    1/(1+sqrt(sum(sim_distances(P1,P2,Tid)))).

sim_distances(P1,P2,Tid) ->
    process_common_prefs(P1,P2,Tid,
                         fun(S1,S2) -> pow(S1-S2,2) end).

-ifdef(EUNIT).
sim_distance_test() ->
    ?assertMatch(0.3483314773547883, sim_distance("Toby","Lisa Rose")).
-endif.


process_common_prefs(P1,P2,Tid,F) ->
    foldl(
      fun({N1,S1},Acc) ->
              try [F(S1,prop_get(N1,ets_lookup(P2,Tid))) | Acc]
              catch _:_ -> Acc end
      end, [], ets_lookup(P1,Tid)).

prop_get(Key, L) ->
    case proplists:get_value(Key,L) of
        undefined -> throw({prop_get,Key});
        Value     -> Value
    end.
    
%% Should be in lists.erl...
take(N,L) ->
    try {R,_} = lists:split(N,L), R
    catch _:_ -> L end.
    

%% -----------------------------------
%% @doc Data representation using ETS.
%%

ets_lookup(Key,Tid) ->
    [{_,Val}] = ets:lookup(Tid,Key),Val.

transform_data2() ->
    transform_data2(data2()).

%% @doc Create a new ETS table where the data is transposed.
%% Input is an existing ETS table identifier containing the input data.
%%
transform_data2(Tid) ->
    ets:foldl(
      fun({K,L},Acc) ->
              lists:foldl(
                fun({A,B},Acc2) -> 
                        ets_append(A,{K,B},Acc2)
                end, Acc, L)
      end, ets:new(?MODULE,[]), Tid).

ets_append(Key,Val,Tid) ->
    case ets:lookup(Tid, Key) of
        [{_,Vals}] -> ets:insert(Tid, {Key,[Val|Vals]});
        _          -> ets:insert(Tid, {Key,[Val]})
    end,
    Tid.
                                                                   
data2() ->
    data2(data()).

%% @doc Create a new ETS table and populate it from the input key-value list.
%%
data2(Data) ->
    Tid = ets:new(?MODULE,[]),
    lists:foreach(fun(X) -> ets:insert(Tid,X) end, Data),
    Tid.
                          

data() ->
    {ok,Data} = file:consult("priv/recommendations.data"),
    Data.
    

            
                            
