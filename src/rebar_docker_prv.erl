-module(rebar_docker_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, docker).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 docker"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "Build docker images efficiently for tests and running a release"},
            {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    TmpDir = ec_file:insecure_mkdtemp(),
    ReleaseName = release_name(State),
    Checksum = checksum(State),

    build_builder_image(ReleaseName, Checksum, TmpDir, State),
    build_runner_image(ReleaseName, Checksum, TmpDir, State),
    build_plt_image(ReleaseName, Checksum, TmpDir, State),

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(no_release_found) ->
    "No release found in configuration";
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%

build_builder_image(ReleaseName, Checksum, Dir, State) ->
    DockerfilePath = filename:join(Dir, "Dockerfile.full"),
    file:write_file(DockerfilePath, dockerfile("erlang:21-alpine", "alpine:3.9", ReleaseName)),
    ProjectDir = rebar_state:dir(State),
    Cmd = builder_image(ReleaseName, Checksum, DockerfilePath, ProjectDir),
    rebar_utils:sh(Cmd, [use_stdout, abort_on_error]).

build_runner_image(ReleaseName, Checksum, Dir, State) ->
    DockerfilePath = filename:join(Dir, "Dockerfile.full"),
    file:write_file(DockerfilePath, dockerfile("erlang:21-alpine", "alpine:3.9", ReleaseName)),
    ProjectDir = rebar_state:dir(State),
    Checksum = checksum(State),
    GitRef = string:trim(os:cmd("git rev-parse HEAD")),
    Cmd = runner_image(ReleaseName, Checksum, GitRef, DockerfilePath, ProjectDir),
    rebar_utils:sh(Cmd, [use_stdout, abort_on_error]).

build_plt_image(ReleaseName, Checksum, Dir, State) ->
    DockerfilePath = filename:join(Dir, "Dockerfile.plt"),
    file:write_file(DockerfilePath, plt_dockerfile("erlang:21-alpine")),
    ProjectDir = rebar_state:dir(State),
    Checksum = checksum(State),
    Cmd = plt_image(ReleaseName, Checksum, DockerfilePath, ProjectDir),
    rebar_utils:sh(Cmd, [use_stdout, abort_on_error]).

release_name(State) ->
    Relx = rebar_state:get(State, relx, []),
    case lists:keyfind(release, 1, Relx) of
        {release, {Name, _Version}, _Apps} ->
            atom_to_list(Name);
        _ ->
            throw({error, {?MODULE, no_release_found}})
    end.

checksum(State) ->
    Opts = rebar_state:opts(State),
    Locks = rebar_opts:get(Opts, {locks, default}),
    ErlOpts = lists:sort(rebar_opts:get(Opts, erl_opts, [])),
    integer_to_list(erlang:phash2([ErlOpts, Locks])).

builder_image(ImageName, Checksum, Dockerfile, Dir) ->
    ["docker build --cache-from=", ImageName, "_builder:", Checksum,
     " --target builder -t ", ImageName, "_builder:", Checksum, " -f ", Dockerfile, " ", Dir].

runner_image(ImageName, Checksum, GitRef, Dockerfile, Dir) ->
    ["docker build --cache-from=", ImageName, "_builder:", Checksum, " --cache-from=",
     ImageName, ":", GitRef, " --target runner -t ", ImageName, ":", GitRef, " -f ", Dockerfile, " ", Dir].

plt_image(ImageName, Checksum, Dockerfile, Dir) ->
    ["docker build --cache-from=", ImageName, "_builder:", Checksum, " --cache-from=",
     ImageName, "_plt:", Checksum, " -t ", ImageName, "_plt:", Checksum, " -f ", Dockerfile, " ", Dir].

dockerfile(BaseImage, RunnerBaseImage, Release) ->
    ["FROM ", BaseImage, " as builder

# git for fetching non-hex depenencies
# tar for unpacking the target system
RUN apk add --no-cache tar git

WORKDIR /src

# build and cache dependencies as their own layer
COPY rebar.config rebar.lock /src/
RUN rebar3 compile

FROM builder as releaser

# copy in the source and build the release tarball
COPY . /src
RUN rebar3 as prod tar

# unpack tarball to be copied into the image built next
RUN mkdir -p /opt/rel
RUN tar -zxvf /src/_build/prod/rel/*/*.tar.gz -C /opt/rel

FROM ", RunnerBaseImage, " as runner

# install openssl, needed by the crypto app
RUN apk add --no-cache openssl ncurses

WORKDIR /opt/", Release, "

COPY --from=releaser /opt/rel /opt/", Release, "

ENV COOKIE ", Release, "
# write files generated during startup to /tmp
ENV RELX_OUT_FILE_PATH /tmp
ENV HOME /opt/", Release, "/bin

ENTRYPOINT [\"/opt/", Release, "/bin/", Release, "\"]

CMD [\"foreground\"]"].

plt_dockerfile(BaseImage) ->
    ["FROM ", BaseImage, " as plter

# git for fetching non-hex depenencies
RUN apk add --no-cache git

WORKDIR /src

# build and cache dependencies as their own layer
COPY rebar.config rebar.lock /src/
RUN rebar3 compile

RUN rebar3 dialyzer --plt-location $HOME/.cache/rebar3 --plt-prefix deps --base-plt-prefix otp

ENTRYPOINT [\"rebar3\"]

CMD [\"dialyzer\", \"--plt-location\", \"$HOME/.cache/rebar3\", \"--plt-prefix\", \"deps\", \"--base-plt-prefix\", \"otp\"]
"].
