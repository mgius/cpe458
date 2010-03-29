#! /bin/bash
## Stupid F# is pulling a ton of crap into these folders.  Clean em out.

find . -iname '*.dll' -o -iname '*.exe' -print -exec rm -f {} \;
