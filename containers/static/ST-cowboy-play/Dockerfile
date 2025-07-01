# Use official Erlang base image
FROM erlang:25

# Set working directory
WORKDIR /app

# Install Rebar3 (Erlang build tool)
RUN wget https://github.com/erlang/rebar3/releases/download/3.20.0/rebar3 && \
    chmod +x rebar3 && \
    mv rebar3 /usr/local/bin/

# Create project structure
RUN mkdir -p src
COPY src/ /app/src/

# Copy configuration files
COPY rebar.config .

# Install dependencies and build
RUN rebar3 get-deps && \
    rebar3 compile

# Create release
RUN rebar3 as prod release

# Expose the default Cowboy port
EXPOSE 8080

# Run the Cowboy server
CMD ["_build/prod/rel/simple_cowboy_app/bin/simple_cowboy_app", "foreground"]