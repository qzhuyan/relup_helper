-define(LOG(LEVEL, FORMAT, ARGS),
        rebar_api:LEVEL("[relup_helper] " ++ FORMAT, ARGS)).
