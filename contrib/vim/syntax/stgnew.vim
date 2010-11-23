" Vim syntax file
" Language:     StGit 'stg new' commit message file
" Author:       Zane Bitter <zane.bitter@alliedtelesis.co.nz>

if exists("b:current_syntax")
  finish
endif


syn case match
syn sync minlines=50


if has("spell")
  syn spell toplevel
endif


syn match    stgnewFirstLine    "\%^.*" nextgroup=stgnewSTG,stgnewComment,stgnewBlank skipnl
syn match    stgnewSummary      "^.\{0,50\}" contained containedin=stgnewFirstLine nextgroup=stgnewOverflow contains=@Spell
syn match    stgnewOverflow     ".*" contained contains=@Spell
syn match    stgnewBlank        "^.\+" contained contains=@Spell
syn match    stgnewSTG          "^STG:.*"
syn match    stgnewComment      "^#.*"

hi def link  stgnewSummary      Keyword
hi def link  stgnewComment      Comment
hi def link  stgnewSTG          Comment
hi def link  stgnewBlank        Error


let b:current_syntax = "stgnew"
