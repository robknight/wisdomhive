{application, wh_app,
  [{description, "Wisdom Hive Prediction Market Server"},
    {vsn, "0.01"},
    {modules, [
  		whbot,
			wh,
  		whmarket,
  		whjson,
  		wh_supervisor
	  ]},
    {registered, []},
    {mod, {wh, []}},
    {env, []},
    {applications, [kernel, stdlib, exmpp]}
  ]
}.

