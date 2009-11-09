%% This is the application resource file (.app file) for the chatyeo,
%% application.
{application, beerenthusiasts_db, 
  [{description, "Beer Enthusiasts DB Backend"},
   {vsn, "0.1.0"},
   {modules, [beerenthusiasts_db_app,   
              beerenthusiasts_db_sup,   
              be_db_connection_server,             
              be_db_interface
              ]},
   {registered,[beerenthusiasts_db_sup, p1]},
   {applications, [kernel, stdlib, sasl, gas, mysql]},
   {mod, {beerenthusiasts_db_app,[]}},
   {start_phases, []}]}.

