#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
SetTitleMatchMode, RegEx


!^o::
    WinActivate, i) Obsidian

    Run "obsidian://advanced-uri?vault=documents&commandname=QuickAdd: Add task"
Return

!^i::
    WinActivate, i) Obsidian

    Run "obsidian://advanced-uri?vault=documents&commandname=QuickAdd: Inbox"
Return

!^d::
    WinActivate, i) Obsidian

    Run "obsidian://advanced-uri?vault=documents&daily=true"
Return

