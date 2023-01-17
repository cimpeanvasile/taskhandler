-module(task_list).

-export([
    ordered/1,
    to_script/1
]).

ordered(#{<<"tasks">> := TaskList}) ->
    {TaskMap, Digraph} = to_internal_structure(TaskList),
    case digraph_utils:topsort(Digraph) of
        false ->
            digraph:delete(Digraph),
            {error, unsortable};
        ReverseOrderedTaskNames ->
            digraph:delete(Digraph),
            {ok, #{<<"tasks">> => to_task_list(ReverseOrderedTaskNames, TaskMap)}}
    end.

to_script(#{<<"tasks">> := TaskList}) ->
    lists:foldl(
        fun(Task, Script) ->
            Command = maps:get(<<"command">>, Task),
            unicode:characters_to_binary([Script, "\n", Command], utf8)
        end,
        "#!/usr/bin/env bash",
        TaskList
    ).

to_internal_structure(TaskList) ->
    lists:foldl(
        fun(Task, {TaskMap, Digraph}) ->
            TaskName = maps:get(<<"name">>, Task),
            Deps = maps:get(<<"requires">>, Task, []),

            digraph:add_vertex(Digraph, TaskName),
            lists:foreach(
                fun(Dep) ->
                    digraph:add_vertex(Digraph, Dep),
                    digraph:add_edge(Digraph, TaskName, Dep)
                end,
                Deps
            ),

            Task1 = maps:without([<<"requires">>], Task),

            {maps:put(TaskName, Task1, TaskMap), Digraph}
        end,
        {#{}, digraph:new()},
        TaskList
    ).

to_task_list(ReverseOrderedTaskNames, TaskMap) ->
    ReverseOrderedTasks = lists:map(
        fun(TaskName) ->
            maps:get(TaskName, TaskMap)
        end,
        ReverseOrderedTaskNames
    ),
    lists:reverse(ReverseOrderedTasks).
