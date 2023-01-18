FROM erlang:25
ADD . /opt/taskhandler
WORKDIR /opt/taskhandler
EXPOSE 8080

CMD ["rebar3", "shell"]