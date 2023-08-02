%%% @doc
%%% Vanillae Erlang Aeternity application supervisor
%%%
%%% The very top level supervisor in the system. It only has one service branch: the
%%% "vanillae_man" (Vanillae Manager).
%%%
%%% See: http://erlang.org/doc/design_principles/applications.html
%%% See: http://zxq9.com/archives/1311
%%% @end

-module(vanillae_sup).
-vsn("0.2.0").
-behaviour(supervisor).
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("GPL-3.0-or-later").

-export([start_link/0]).
-export([init/1]).


-spec start_link() -> {ok, pid()}.
%% @private
%% This supervisor's own start function.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
%% @private
%% The OTP init/1 function.

init([]) ->
    RestartStrategy = {one_for_one, 0, 60},
    Manager   = {vanillae_man,
                 {vanillae_man, start_link, []},
                 permanent,
                 5000,
                 worker,
                 [vanillae_man]},
    Children  = [Manager],
    {ok, {RestartStrategy, Children}}.
