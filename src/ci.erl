%%%-------------------------------------------------------------------
%%% Created :  1 Apr 2010 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%%
%%% @doc Examples from the O'Reilly book: "Collective Intelligence"
%%%
%%%-------------------------------------------------------------------
-module(ci).
-export([get_u_item/0
        ,get_u_data/0
        ,get_blogdata/0
        ]).

get_u_item()   -> zip_read_file("priv/u.item.zip").
get_u_data()   -> zip_read_file("priv/u.data.zip").
get_blogdata() -> zip_read_file("priv/blogdata.txt.zip").

zip_read_file(Fname) ->
    {ok,Zbin}=file:read_file(Fname),
    {ok,[{_Fname2,Bin}]} = zip:unzip(Zbin,[memory]),
    {ok,Bin}.

