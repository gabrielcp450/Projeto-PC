%% Game constants
-define(TICK, 1).                     %% milliseconds per update
-define(FRICTION, 20.0).
-define(ACCELERATION, 20.0).
-define(MODIFIERS_INTERVAL, 5000).             %% milliseconds between modifiers
-define(BUFF, 100).                  %% millisenconds to each tick to buff
-define(MAX_MODIFIERS, 2).           %% max per modifier type
-define(PLAYER_RADIUS, 0.025).
-define(MODIFIER_RADIUS, 0.015).
-define(PROJECTILE_RADIUS, 0.005).
-define(PROJECTILE_VELOCITY_MAX, 2).
-define(PROJECTILE_VELOCITY_MIN, 0.25).
-define(PROJECTILE_VELOCITY_INITIAL, 1).
-define(PROJECTILE_INTERVAL_MAX, 2).
-define(PROJECTILE_INTERVAL_MIN, 0.25).
-define(PROJECTILE_INTERVAL_INITIAL, 1).
-define(VELOCITY_TICK_BUFF, 0.00005).
-define(INTERVAL_TICK_BUFF, 0.00005).

%% Player movement keys
-record(keys, {
    w = false,
    s = false,
    a = false,
    d = false
}).

%% Player state
-record(player, {
    p            = {0.0, 0.0},    %% position
    v            = {0.0, 0.0},    %% velocity
    a            = {0.0, 0.0},    %% acceleration
    aim          = {0.0, 0.0},    %% aiming direction
    points       = 0,             %% score
    projs        = #{},           %% projectiles
    proj_v       = ?PROJECTILE_VELOCITY_INITIAL,             %% projectile velocity
    proj_i       = ?PROJECTILE_INTERVAL_INITIAL,             %% projectile interval
    k            = #keys{},       %% current keys pressed
    id           = 0,             %% 0 = Player 1, 1 = Player 2
    reloading    = false
}).

%% Projectile state
-record(proj, {
    p = {0.0, 0.0},  %% position
    v = {0.0, 0.0}   %% velocity
}).
