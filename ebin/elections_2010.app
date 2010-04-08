%%-*- mode: erlang -*-
{application, elections_2010,
 [
  {description, "elections_2010"},
  {vsn, "0.0"},
  {modules, [
             elections_2010,
             elections_2010_app,
             elections_2010_sup,
             elections_2010_resource,
	     votes_collector
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
                  crypto,
                  mochiweb,
                  webmachine,
		  inets,
		  ecouch
                 ]},
  {mod, { elections_2010_app, []}},
  {env, []}
 ]}.
