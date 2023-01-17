-module(task_http_handler).
-behavior(cowboy_handler).

-export([
    init/2,
    allowed_methods/2,
    malformed_request/2,
    content_types_accepted/2,
    receive_tasks_json/2
]).

init(Req, _State) ->
    {cowboy_rest, Req, #{}}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

malformed_request(Req, State) ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"required">> => [<<"tasks">>],
        <<"properties">> => #{
            <<"tasks">> => #{
                <<"type">> => <<"array">>,
                <<"items">> => #{
                    <<"$ref">> => <<"#/$defs/task">>
                }
            }
        },
        <<"$defs">> => #{
            <<"task">> => #{
                <<"type">> => <<"object">>,
                <<"required">> => [<<"name">>, <<"command">>],
                <<"properties">> => #{
                    <<"name">> => #{<<"type">> => <<"string">>},
                    <<"command">> => #{<<"type">> => <<"string">>},
                    <<"requires">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{
                            <<"type">> => <<"string">>
                        }
                    }
                }
            }
        }
    },
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    try jiffy:decode(Body, [return_maps]) of
        Json ->
            case jesse:validate_with_schema(Schema, Json) of
                {ok, Json} ->
                    {false, Req1, maps:put(<<"tasks">>, Json, State)};
                {error, _Errors} ->
                    {true, reply_json(400, #{<<"error">> => <<"invalid_json">>}, Req1), State}
            end
    catch
        _ ->
            {false, reply_json(400, #{<<"error">> => <<"Invalid JSON">>}, Req1), State}
    end.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, receive_tasks_json}], Req, State}.

receive_tasks_json(Req, #{<<"tasks">> := Tasks} = State) ->
    case task_list:ordered(Tasks) of
        {error, unsortable} ->
            {true, reply_json(200, #{<<"error">> => <<"unsortable">>}, Req), State};
        {ok, OrderedTasks} ->
            case cowboy_req:match_qs([{format, [], <<"json">>}], Req) of
                #{format := <<"json">>} ->
                    {true, reply_json(200, OrderedTasks, Req), State};
                #{format := <<"bash">>} ->
                    Script = task_list:to_script(OrderedTasks),
                    {true, reply_bash(Script, Req), State};
                _ ->
                    {false, reply_json(400, #{<<"error">> => <<"invalid_format_param">>}, Req), State}
            end
    end.

reply_json(Code, Body, Req) ->
    cowboy_req:reply(
        Code,
        #{<<"content-type">> => <<"application/json">>},
        jiffy:encode(Body),
        Req
    ).

reply_bash(Body, Req) ->
    cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"text/x-shellscript">>},
        Body,
        Req
    ).
