" Vim syntax file
" Language:     StGit 'stg squash' commit message file
" Author:       Zane Bitter <zane.bitter@alliedtelesis.co.nz>

if exists("b:current_syntax")
  finish
endif


syn case match
syn sync minlines=50


if has("spell")
  syn spell toplevel
endif


syn match    stgsqFirstLine     "\%^.*" nextgroup=stgsqComment,stgsqContext,stgsqBlank skipnl
syn match    stgsqSummary       "^.\{0,50\}" contained containedin=stgsqFirstLine nextgroup=stgsqOverflow contains=@Spell
syn match    stgsqOverflow      ".*" contained contains=@Spell
syn match    stgsqBlank         "^.\+" contained contains=@Spell
syn match    stgsqSeparator     "-\+$" contained
syn region   stgsqPatch         start="^\(.\{66\}-\{4\}$\)\@=" end="\(^.\{66\}-\{4\}$\)\@=" contained containedin=stgsqContext contains=@Spell fold
syn match    stgsqNextPatch     "^.\{66\}-\{4\}$" contained containedin=stgsqPatch contains=stgsqPatchName
syn match    stgsqPatchName     "^.\{-\}\(-*$\)\@=" contained containedin=stgsqNextPatch nextgroup=stgsqSeparator
syn region   stgsqContext       start="^---" end="%$" contains=@Spell fold
syn match    stgsqComment       "^#.*"

hi def link  stgsqSummary       Keyword
hi def link  stgsqComment       Comment
hi def link  stgsqBlank         Error
hi def link  stgsqContext       Comment
hi def link  stgsqPatch         Constant
hi def link  stgsqPatchName     Identifier
hi def link  stgsqSeparator     Comment


let b:current_syntax = "stgsquash"
