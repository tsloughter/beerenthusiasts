%% This is the application resource file (.app file) for the beerenthusiasts,
%% application.
{application, beerenthusiasts, 
  [{description, "Beer Enthusiasts main app"},
   {vsn, "0.1.0"},
   {modules, [beerenthusiasts_app,
              beerenthusiasts_sup,
              beerenthusiasts,
              
              be_user_sup,
              be_user_server,

              be_recipes_utils,
              erls3]},
   {registered,[beerenthusiasts_sup, erls3]},
   {applications, [kernel, stdlib, beerenthusiasts_db, couchbeam]},
   {mod, {beerenthusiasts_app,[]}},
   {start_phases, []}]}.

