" Vim syntax file
" Language:     StGit 'stg mail' file
" Author:       Zane Bitter <zane.bitter@alliedtelesis.co.nz>

if exists("b:current_syntax")
  finish
endif


runtime! syntax/mail.vim
unlet b:current_syntax

syn include @stgDiff syntax/diff.vim


syn case match
syn sync minlines=50


if has("spell")
  syn spell toplevel
endif


syn match    stgmailComment     "^#.*"
syn region   stgmailDiffs       start="^---" end="%$" contains=@stgDiff fold
syn region   stgmailDiff        start="^\%(diff --git \)\@=" end="^\%(diff --git \|$\)\@=" contained containedin=stgmailDiffs contains=@stgDiff fold

hi def link  stgmailComment     Comment


let b:current_syntax = "stgmail"
