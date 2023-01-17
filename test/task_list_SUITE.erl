-module(task_list_SUITE).

-export([
    all/0,
    orders_task_list_correctly/1,
    keeps_order_when_no_sorting_needed/1,
    returns_error_when_cycle_detected/1,
    returns_script/1
]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
        orders_task_list_correctly,
        keeps_order_when_no_sorting_needed,
        returns_error_when_cycle_detected,
        returns_script
    ].

orders_task_list_correctly(_Config) ->
    Tasks = #{
        <<"tasks">> => [
            #{
                <<"command">> => <<"touch /tmp/file1">>,
                <<"name">> => <<"task-1">>
            },
            #{
                <<"command">> => <<"cat /tmp/file1">>,
                <<"name">> => <<"task-2">>,
                <<"requires">> => [<<"task-3">>]
            },
            #{
                <<"command">> => <<"echo 'Hello World!' > /tmp/file1">>,
                <<"name">> => <<"task-3">>,
                <<"requires">> => [<<"task-1">>]
            },
            #{
                <<"command">> => <<"rm /tmp/file1">>,
                <<"name">> => <<"task-4">>,
                <<"requires">> => [<<"task-2">>, <<"task-3">>]
            }
        ]
    },
    ExpectedOrderedList = #{
        <<"tasks">> => [
            #{
                <<"name">> => <<"task-1">>,
                <<"command">> => <<"touch /tmp/file1">>
            },
            #{
                <<"name">> => <<"task-3">>,
                <<"command">> => <<"echo 'Hello World!' > /tmp/file1">>
            },
            #{
                <<"name">> => <<"task-2">>,
                <<"command">> => <<"cat /tmp/file1">>
            },
            #{
                <<"name">> => <<"task-4">>,
                <<"command">> => <<"rm /tmp/file1">>
            }
        ]
    },
    {ok, ExpectedOrderedList} = task_list:ordered(Tasks).

keeps_order_when_no_sorting_needed(_Config) ->
    Tasks = #{
        <<"tasks">> => [
            #{
                <<"command">> => <<"touch /tmp/file1">>,
                <<"name">> => <<"task-1">>
            },
            #{
                <<"command">> => <<"cat /tmp/file1">>,
                <<"name">> => <<"task-2">>
            },
            #{
                <<"command">> => <<"echo 'Hello World!' > /tmp/file1">>,
                <<"name">> => <<"task-3">>
            }
        ]
    },
    ExpectedOrderedList = #{
        <<"tasks">> => [
            #{
                <<"name">> => <<"task-1">>,
                <<"command">> => <<"touch /tmp/file1">>
            },
            #{
                <<"name">> => <<"task-2">>,
                <<"command">> => <<"cat /tmp/file1">>
            },
            #{
                <<"name">> => <<"task-3">>,
                <<"command">> => <<"echo 'Hello World!' > /tmp/file1">>
            }
        ]
    },
    {ok, ExpectedOrderedList} = task_list:ordered(Tasks).

returns_error_when_cycle_detected(_Config) ->
    Tasks = #{
        <<"tasks">> => [
            #{
                <<"command">> => <<"touch /tmp/file1">>,
                <<"name">> => <<"task-1">>,
                <<"requires">> => [<<"task-2">>]
            },
            #{
                <<"command">> => <<"cat /tmp/file1">>,
                <<"name">> => <<"task-2">>,
                <<"requires">> => [<<"task-3">>]
            },
            #{
                <<"command">> => <<"echo 'Hello World!' > /tmp/file1">>,
                <<"name">> => <<"task-3">>,
                <<"requires">> => [<<"task-1">>]
            }
        ]
    },
    {error, unsortable} = task_list:ordered(Tasks).

returns_script(_Config) ->
    Tasks =
        #{
            <<"tasks">> => [
                #{
                    <<"name">> => <<"task-1">>,
                    <<"command">> => <<"touch /tmp/file1">>
                },
                #{
                    <<"name">> => <<"task-3">>,
                    <<"command">> => <<"echo 'Hello World!' > /tmp/file1">>
                },
                #{
                    <<"name">> => <<"task-2">>,
                    <<"command">> => <<"cat /tmp/file1">>
                },
                #{
                    <<"name">> => <<"task-4">>,
                    <<"command">> => <<"rm /tmp/file1">>
                }
            ]
        },
    ExpectedScript =
        <<"#!/usr/bin/env bash\ntouch /tmp/file1\necho 'Hello World!' > /tmp/file1\ncat /tmp/file1\nrm /tmp/file1">>,
    ExpectedScript = task_list:to_script(Tasks).
