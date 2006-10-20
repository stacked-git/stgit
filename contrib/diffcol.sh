#!/bin/sh

# Code copied from Quilt (http://savannah.nongnu.org/projects/quilt)
#
# Copyright 2006 - the Quilt authors
#
#  This script is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License version 2 as
#  published by the Free Software Foundation.

setup_colors()
{
	local C="diffhdr=1;36:diffhdradd=1;32:diffadd=32:diffhdrmod=1;35:diffmod=35:diffhdrrem=1;31:diffrem=31:diffhunk=36:diffctx=34:diffcctx=33:default=0"
	[ -n "$DIFF_COLORS" ] && C="$C:$DIFF_COLORS"

	C=${C//=/=\'$'\e'[}
	C=col${C//:/m\'; col}m\'
	#coldefault=$(tput op)
	eval $C
}

setup_colors

gawk '{
	if (/^(Index:|diff --git) /)
		print "'$coldiffhdr'" $0 "'$coldefault'"
	else if (/^======*$/)
		print "'$coldiffhdr'" $0 "'$coldefault'"
	else if (/^\+\+\+/)
		print "'$coldiffhdradd'" $0 "'$coldefault'"
	else if (/^\*\*\*/)
		print "'$coldiffhdrmod'" $0 "'$coldefault'"
	else if (/^---/)
		print "'$coldiffhdrrem'" $0 "'$coldefault'"
	else if (/^(\+|new( file)? mode )/)
		print "'$coldiffadd'" $0 "'$coldefault'"
	else if (/^(-|(deleted file|old) mode )/)
		print "'$coldiffrem'" $0 "'$coldefault'"
	else if (/^!/)
		print "'$coldiffmod'" $0 "'$coldefault'"
	else if (/^@@ \-[0-9]+(,[0-9]+)? \+[0-9]+(,[0-9]+)? @@/)
		print gensub(/^(@@[^@]*@@)([ \t]*)(.*)/,
			"'$coldiffhunk'" "\\1" "'$coldefault'" \
			"\\2" \
			"'$coldiffctx'" "\\3" "'$coldefault'", "")
	else if (/^\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*/)
		print "'$coldiffcctx'" $0 "'$coldefault'"
	else {
		print
	}
}' $1 | less -R -S
