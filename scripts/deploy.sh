./execute.sh 'cd gaoler; cp ebin/gaoler.app ~/gaoler.app; git pull origin master; rel/gaoler/bin/gaoler stop; rm -rf rel/gaoler; ./rebar get-deps; ./rebar compile; cp ~/gaoler.app ebin/gaoler.app; ./rebar generate; rel/gaoler/bin/gaoler start'
