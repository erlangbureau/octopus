-module(octopus_name_resolver).

-export([get/2]).

-define(BIN(Atom), (atom_to_binary(Atom, utf8))/binary).
-define(ERR(Format, Args), error_logger:error_msg(Format, Args)).

get(PoolId, Server) ->
    try
        binary_to_atom(<<?BIN(Server), "_", ?BIN(PoolId)>>, utf8)
    catch
        _:_ ->
            ?ERR("octopus_name_resolver error when processing PoolId:~p and Server: ~p", [PoolId, Server]),
            Server
    end.
