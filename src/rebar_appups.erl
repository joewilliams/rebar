%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2011 Joe Williams (joe@joetify.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% ------------------------------------------------------------------

-module(rebar_appups).

-include("rebar.hrl").

-export(['generate-appups'/2]).

%% public api

'generate-appups'(_Config, ReltoolFile) ->
    case rebar_config:get_global(previous_release, false) of
        false ->
            ?ABORT("previous_release=PATH is required to "
                   "create upgrade package~n", []);
        OldVerPath ->
            %% Get the new and old release name and versions
            {Name, _Ver} = rebar_utils:get_reltool_release_info(ReltoolFile),
            NewVerPath = filename:join([".", Name]),
            {NewName, NewVer} = rebar_utils:get_rel_release_info(Name, NewVerPath),
            {OldName, OldVer} = rebar_utils:get_rel_release_info(Name, OldVerPath),
            
            %% Run some simple checks
            true = rebar_utils:prop_check(NewVer =/= OldVer, 
                              "New and old .rel versions match~n", []),
            true = rebar_utils:prop_check(NewName == OldName, 
                              "Reltool and .rel release names do not match~n", []),

            %% Get lists of the old and new app files
            OldAppFiles = rebar_utils:find_files(
                            filename:join([OldVerPath, "lib"]), "^.*.app$"),
            NewAppFiles = rebar_utils:find_files(
                            filename:join([NewName, "lib"]), "^.*.app$"),

            %% Find all the apps that have been upgraded
            UpgradedApps = get_upgraded_apps(OldAppFiles, NewAppFiles),

            %% Get a list of any appup files that exist in the new release
            NewAppUpFiles = rebar_utils:find_files(
                              filename:join([NewName, "lib"]), "^.*.appup$"),

            %% Convert the list of appup files into app names
            AppUpApps = lists:map(fun(File) ->
                                          file_to_name(File)
                                  end, NewAppUpFiles),

            %% Create a list of apps that don't have appups already
            GenAppUpApps = genappup_which_apps(UpgradedApps, AppUpApps),
            
            %% Generate appup files
            generate_appups(Name, OldVerPath, GenAppUpApps),
            
            ok
    end.

%% internal api

get_upgraded_apps(OldAppFiles, NewAppFiles) ->
    OldAppsVer = [get_app_version(AppFile) || AppFile <- OldAppFiles],
    NewAppsVer = [get_app_version(AppFile) || AppFile <- NewAppFiles],
    UpgradedApps = lists:subtract(NewAppsVer, OldAppsVer),
    lists:map(
      fun({App, NewVer}) -> 
             {App, OldVer} = proplists:lookup(App, OldAppsVer),
             {App, {OldVer, NewVer}}
      end, 
      UpgradedApps).

get_app_version(File) ->
    case file:consult(File) of
        {ok,[{application, Name,[_,{vsn,Ver}|_]}]} ->
            {Name, Ver};
        _ ->
            ?ABORT("Failed to parse ~s~n", [File])
    end.

file_to_name(File) ->
    Pos1 = string:rchr(File, $/),
    Pos2 = string:rchr(File, $.),
    list_to_atom(string:sub_string(File, Pos1 + 1, Pos2 - 1)).

genappup_which_apps(UpgradedApps, [First|Rest]) ->
    List = proplists:delete(First, UpgradedApps),
    genappup_which_apps(List, Rest);
genappup_which_apps(Apps, []) ->
    Apps.

generate_appups(Name, OldVerPath, [{App, {OldVer, NewVer}}|Rest]) ->
    OldEbinDir = filename:join([".", OldVerPath, "lib",
                             atom_to_list(App) ++ "-" ++ OldVer, "ebin"]),
    NewEbinDir = filename:join([".", Name, "lib",
                             atom_to_list(App) ++ "-" ++ NewVer, "ebin"]),

    {AddedFiles, DeletedFiles, ChangedFiles} = beam_lib:cmp_dirs(NewEbinDir, OldEbinDir),

    AddedInst = generate_instructions({added, AddedFiles}, []),
    DeletedInst = generate_instructions({deleted, DeletedFiles}, []),
    ChangedInst = generate_instructions({changed, ChangedFiles}, []),

    Inst = lists:append([AddedInst, DeletedInst, ChangedInst]),
    AppUpFile = filename:join([NewEbinDir, atom_to_list(App) ++ ".appup"]),
                   
    ok = file:write_file(AppUpFile, 
                         io_lib:fwrite("%% appup generated for ~p by your friend rebar (~p)\n"
                                       "{~p, [{~p, ~p}], [{~p, []}]}.\n",
                                       [App, rebar_utils:now_str(), NewVer, OldVer, Inst, OldVer])),
    ?CONSOLE("Generated appup for ~p~n", [App]),
    generate_appups(Name, OldVerPath, Rest);
generate_appups(_, _, []) ->
    ?CONSOLE("Appup generation complete~n", []).

generate_instructions({added, [First|Rest]}, Acc) ->
    Name = file_to_name(First),
    NewAcc = lists:append([{add_module, Name}], Acc),
    generate_instructions({added, Rest}, NewAcc);
generate_instructions({deleted, [First|Rest]}, Acc) ->
    Name = file_to_name(First),
    NewAcc = lists:append([{delete_module, Name}], Acc),
    generate_instructions({deleted, Rest}, NewAcc);
generate_instructions({changed, [{File, _} |Rest]}, Acc) ->
    {ok, Info} = beam_lib:chunks(File, [attributes, exports]),
    Inst = generate_instructions_advanced(Info),
    NewAcc = lists:append([Inst], Acc),
    generate_instructions({changed, Rest}, NewAcc);
generate_instructions({_, []}, Acc) ->
    Acc.

generate_instructions_advanced({Name, [{attributes, [{behaviour, [supervisor]}|_]}|_]}) ->
    {update, Name, supervisor};
generate_instructions_advanced({Name, [{attributes, [{behavior, [supervisor]}|_]}|_]}) ->
    {update, Name, supervisor};
generate_instructions_advanced({Name, [_, {exports, Exports}|_]}) ->
    case proplists:is_defined(code_change, Exports) of
        true ->
            {update, Name, {advanced, []}};
        _ ->
            {load_module, Name}
    end.




