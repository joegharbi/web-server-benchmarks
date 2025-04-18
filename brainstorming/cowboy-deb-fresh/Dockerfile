FROM erlang:26
WORKDIR /app
RUN apt-get update && apt-get install -y make git
RUN wget https://erlang.mk/erlang.mk && chmod +x erlang.mk
COPY Makefile .
COPY src ./src
COPY relx.config .
RUN make && make rel
EXPOSE 8080
CMD ["_rel/my_server/bin/my_server", "foreground"]
