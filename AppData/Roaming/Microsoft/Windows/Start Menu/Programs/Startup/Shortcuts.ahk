#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
SetTitleMatchMode, RegEx


; Obsidian Shortcuts

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



; Workspaces Shortcuts - Extracted from https://www.computerhope.com/tips/tip224.htm

DesktopCount = 2 
CurrentDesktop = 1 
mapDesktopsFromRegistry() {
	global CurrentDesktop, DesktopCount
	IdLength := 32
	SessionId := getSessionId()
	if (SessionId) {
		RegRead, CurrentDesktopId, HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\SessionInfo\%SessionId%\VirtualDesktops, CurrentVirtualDesktop
		if (CurrentDesktopId) {
			IdLength := StrLen(CurrentDesktopId)
		}
	}
	RegRead, DesktopList, HKEY_CURRENT_USER, SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\VirtualDesktops, VirtualDesktopIDs
	if (DesktopList) {
		DesktopListLength := StrLen(DesktopList)
		DesktopCount := DesktopListLength / IdLength
	} else {
		DesktopCount := 1
	}
	i := 0
	while (CurrentDesktopId and i < DesktopCount) {
		StartPos := (i * IdLength) + 1
		DesktopIter := SubStr(DesktopList, StartPos, IdLength)
		OutputDebug, The iterator is pointing at %DesktopIter% and count is %i%.
		if (DesktopIter = CurrentDesktopId) {
			CurrentDesktop := i + 1
			OutputDebug, Current desktop number is %CurrentDesktop% with an ID of %DesktopIter%.
			break
		}
		i++
	}
}

getSessionId() {
	ProcessId := DllCall("GetCurrentProcessId", "UInt")
	if ErrorLevel {
		OutputDebug, Error getting current process id: %ErrorLevel%
		return
	}
	OutputDebug, Current Process Id: %ProcessId%
	DllCall("ProcessIdToSessionId", "UInt", ProcessId, "UInt*", SessionId)
	if ErrorLevel {
		OutputDebug, Error getting session id: %ErrorLevel%
		return
	}
	OutputDebug, Current Session Id: %SessionId%
	return SessionId
}

switchDesktopByNumber(targetDesktop) {
	global CurrentDesktop, DesktopCount
	mapDesktopsFromRegistry()
	if (targetDesktop > 9 || targetDesktop < 1) {
		OutputDebug, [invalid] target: %targetDesktop% current: %CurrentDesktop%
		return
	}
	
	if (targetDesktop > DesktopCount) {
		OutputDebug, [right] desktop dont exist: %targetDesktop%. Creating one.
		while (DesktopCount < targetDesktop) {
			createVirtualDesktop()
		}
		return
	}

	while(CurrentDesktop < targetDesktop) {
		Send ^#{Right}
		CurrentDesktop++
		OutputDebug, [right] target: %targetDesktop% current: %CurrentDesktop%
	}

	while(CurrentDesktop > targetDesktop) {
		Send ^#{Left}
		CurrentDesktop--
		OutputDebug, [left] target: %targetDesktop% current: %CurrentDesktop%
	}
}

createVirtualDesktop() {
	global CurrentDesktop, DesktopCount
	Send, #^d
	DesktopCount++
	CurrentDesktop = %DesktopCount%
	OutputDebug, [create] desktops: %DesktopCount% current: %CurrentDesktop%
}

deleteVirtualDesktop() {
	global CurrentDesktop, DesktopCount
	Send, #^{F4}
	if (DesktopCount > 1) {
		DesktopCount--
		CurrentDesktop--
	}
	OutputDebug, [delete] desktops: %DesktopCount% current: %CurrentDesktop%
}

SetKeyDelay, 75
mapDesktopsFromRegistry()
OutputDebug, [loading] desktops: %DesktopCount% current: %CurrentDesktop%

LWin & 1::switchDesktopByNumber(1)
LWin & 2::switchDesktopByNumber(2)
LWin & 3::switchDesktopByNumber(3)
LWin & 4::switchDesktopByNumber(4)
LWin & 5::switchDesktopByNumber(5)
LWin & 6::switchDesktopByNumber(6)
LWin & 7::switchDesktopByNumber(7)
LWin & 8::switchDesktopByNumber(8)
LWin & 9::switchDesktopByNumber(9)
LWin & 0::deleteVirtualDesktop()

