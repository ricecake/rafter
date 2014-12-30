-module(rafter_backend).

%% init backend
-callback init(Args :: term()) ->
    {ok, State :: term()} | {error, Reason :: term()}.

%% read op
-callback read(Operation :: term(), State :: term()) ->
    {Value :: term(), State :: term()}.

%% write op
-callback write(Operation :: term(), State :: term()) ->
    {Value :: term(), State :: term()}.

%% stop backend
-callback stop(State :: term()) ->
    ok | {error, Reason :: term()}.
