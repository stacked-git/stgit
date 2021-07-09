" Vim syntax file
" Language:     StGit 'stg edit' commit message file
" Author:       Zane Bitter <zane.bitter@alliedtelesis.co.nz>

if exists("b:current_syntax")
  finish
endif

syn include @stgDiff syntax/diff.vim


syn case match
syn sync minlines=50


if has("spell")
  syn spell toplevel
endif


syn region   stgeditHeaders     start="\%^\S\+:" end="^$" nextgroup=stgeditFirstLine skipempty contains=stgeditHeaderKey,stgeditEmailAddr
syn match    stgeditHeaderKey   "^\S\+:" contained
syn match    stgeditEmailAddr   "<.\{-}>" contained contains=@NoSpell
syn match    stgeditFirstLine   "^.\+" contained nextgroup=stgeditDiffs,stgeditComment,stgeditBlank skipnl
syn match    stgeditSummary     "^.\{0,50\}" contained containedin=stgeditFirstLine nextgroup=stgeditOverflow contains=@Spell
syn match    stgeditOverflow    ".*" contained contains=@Spell
syn match    stgeditBlank       "^.\+" contained contains=@Spell
syn match    stgeditComment     "^#.*"
syn region   stgeditDiffs       start="^---" end="%$" contains=@stgDiff fold
syn region   stgeditDiff        start="^\%(diff --git \)\@=" end="^\%(diff --git \|$\)\@=" contained containedin=stgeditDiffs contains=@stgDiff fold

hi def link  stgeditHeaderKey   Type
hi def link  stgeditEmailAddr   Special
hi def link  stgeditSummary     Keyword
hi def link  stgeditComment     Comment
hi def link  stgeditBlank       Error


let b:current_syntax = "stgedit"
