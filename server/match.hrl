%% Player movement keys
-record(keys, {
    w = false,
    s = false,
    a = false,
    d = false
}).

%% Player state
-record(player, {
    p       = {0.0, 0.0},    %% position
    v       = {0.0, 0.0},    %% velocity
    a       = {0.0, 0.0},    %% acceleration
    aim     = {0.0, 0.0},    %% aiming direction
    points  = 0,             %% score
    proj_v  = 0,             %% projectile velocity
    proj_i  = 0,             %% projectile interval
    k       = #keys{},       %% current keys pressed
    id      = 0              %% 0 = Player 1, 1 = Player 2
}).

%% Projectile state
-record(proj, {
    p = {0.0, 0.0},  %% position
    v = {0.0, 0.0}   %% velocity
}).

%% Game constants
-define(TICK, 1).                     %% milliseconds per update
-define(FRICTION, 20.0).
-define(ACCELERATION, 20.0).
-define(MODIFIERS, 100).             %% milliseconds between modifiers
-define(MAX_MODIFIERS, 2).           %% max per modifier type
-define(PLAYER_RADIUS, 0.025).
-define(PROJECTILE_RADIUS, 0.005).