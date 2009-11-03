%% This is the application resource file (.app file) for the beerenthusiasts,
%% application.
{application, beerenthusiasts, 
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [beerenthusiasts_app,
              beerenthusiasts_sup,
              beerenthusiasts]},
   {registered,[beerenthusiasts_sup]},
   {applications, [kernel, stdlib]},
   {mod, {beerenthusiasts_app,[]}},
   {start_phases, []}]}.

