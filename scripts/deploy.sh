#!/bin/sh

# prevent new release overwriting the node settings with repo defaults
./execute.sh 'cd ~emdc/gaoler && cp ebin/gaoler.app ~/gaoler.app'


# stop server, delete old release (if present)
./execute.sh 'cd ~emdc/gaoler && rel/gaoler/bin/gaoler stop'
./execute.sh 'cd ~emdc/gaoler && rm -rf rel/gaoler'

# rebuild
./execute.sh 'cd ~emdc/gaoler && git pull origin master && ./rebar get-deps && ./rebar compile'

# prevent new release overwriting the node settings with repo defaults
./execute.sh 'cd ~emdc/gaoler && cp ~/gaoler.app ebin/gaoler.app'

# generate and run the release
# ./execute.sh 'cd ~emdc/gaoler && ./rebar generate && rel/gaoler/bin/gaoler start'
