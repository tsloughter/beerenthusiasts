{application, beerenthusiasts_web, [
	{description,  "Nitrogen Website"},
  {vsn, "0.1.0"},        
	{mod, {beerenthusiasts_web_app, []}},
  {modules, [beerenthusiasts_web_app, 
             web_index
   ]},             
  {registered,[beerenthusiasts_web]},        
  {applications, [kernel, stdlib, sasl, gas, ewlib, beerenthusiasts]},
	{env, [
		{platform, inets}, %% {inets|yaws|mochiweb}
		{port, 8000},
		{session_timeout, 20},
		{sign_key, "SIGN_KEY"},
		{www_root, "./wwwroot"}
	]}
]}.
