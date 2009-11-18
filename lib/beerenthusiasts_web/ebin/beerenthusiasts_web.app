{application, beerenthusiasts_web, [
	{description,  "Nitrogen Website"},
  {vsn, "0.1.0"},        
	{mod, {beerenthusiasts_web_app, []}},
  {modules, [beerenthusiasts_web_app, 
             web_index,
             web_queue,            
             web_profile,
             web_comments,
             web_ratings
   ]},             
  {registered,[beerenthusiasts_web]},        
  {applications, [kernel, stdlib, sasl, gas, ewlib, beerenthusiasts]}
]}.
