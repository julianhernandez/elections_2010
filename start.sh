#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin -boot start_sasl -s reloader -s elections_2010
