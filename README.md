taskhandler
=====

A simple OTP application that basically does a topological sort on a list of tasks, written using Cowboy

Run with docker
-----

    $ docker compose up run

Run with OTP (OTP 25 was used for development)
-----

    $ rebar3 shell


Ordering a list of tasks
-----

    $ curl --location --request POST 'localhost:8080' \
        --header 'Content-Type: application/json' \
        --data-raw '{
            "tasks": [
                {
                    "name": "task-1",
                    "command": "touch /tmp/file1"
                },
                {
                    "name": "task-2",
                    "command": "cat /tmp/file1",
                    "requires": [
                        "task-3"
                    ]
                },
                {
                    "name": "task-3",
                    "command": "echo '\''Hello World!'\'' > /tmp/file1",
                    "requires": [
                        "task-1"
                    ]
                },
                {
                    "name": "task-4",
                    "command": "rm /tmp/file1",
                    "requires": [
                        "task-2",
                        "task-3"
                    ]
                }
            ]
        }'

Requesting the ordered list of tasks as a bash script
-----

To retrieve the list of tasks as a bash script, simply add a `format=bash` query parameter

    $ curl --location --request POST 'localhost:8080?format=bash' \
        --header 'Content-Type: application/json' \
        --data-raw '{
            "tasks": [
                {
                    "name": "task-1",
                    "command": "touch /tmp/file1"
                },
                {
                    "name": "task-2",
                    "command": "cat /tmp/file1",
                    "requires": [
                        "task-3"
                    ]
                },
                {
                    "name": "task-3",
                    "command": "echo '\''Hello World!'\'' > /tmp/file1",
                    "requires": [
                        "task-1"
                    ]
                },
                {
                    "name": "task-4",
                    "command": "rm /tmp/file1",
                    "requires": [
                        "task-2",
                        "task-3"
                    ]
                }
            ]
        }'

Future improvements
-----

- decide response format based on `Accept` header
- move JSON schema validation into the `task_list` module
- add tests for schema validation
- add automated tests for the http endpoint

Development
-----

For more portability, development can be done using docker. Just spin up the dev box using docker compose, the taskhandler folder
will be mounted under /taskhandler. Rebar can then be used to compile the project, start it up or run tests

    $ docker compose up dev