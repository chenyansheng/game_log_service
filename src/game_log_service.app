{ 
    application, game_log_service,
    [ 
        {description, "Game Log Service Application."},
        {vsn, "1.0"},
        {registered, [game_log_service]},
        {applications, [kernel, stdlib, sasl]},
        {mod, {game_log_service, []}} 
    ]
}.
