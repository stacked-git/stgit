# modify PS1 to your preference and include this file in your bashrc
# or copy to /etc/bash_completions.d.

if [ "$PS1" ]; then
	function __prompt_git()
	{
		local br top
		br=$(stg branch 2>/dev/null) || return
		top=$(stg top 2>/dev/null) || return
		echo "[$top@$br]"
	}
	PS1='\u@\h:$(__prompt_git)\W\$ '
fi
