@echo off

if defined EMACS (
   set emacs=%EMACS%
)
if not defined EMACS (
   set emacs=emacs
)

%emacs% emacs --batch -l %~dp0keg -- %*
