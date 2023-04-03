'#Define UNICODE
#include once "windows.bi"
#Include Once "win/commctrl.bi"
#Include Once "win/shellapi.bi"
#Include Once "WinMain.bi"

Function WndProc(ByVal hWin As HWND,ByVal uMsg As UINT,ByVal wParam As WPARAM,ByVal lParam As LPARAM) As Integer

	Select Case uMsg
		Case WM_INITDIALOG
			hWndMain = hWin
			Dim As Integer cx = GetSystemMetrics( SM_CXFULLSCREEN )   ' 
            Dim As Integer cy = GetSystemMetrics( SM_CYFULLSCREEN )   ' 
			MoveWindow hWin, (cx - 1000) / 2,(cy - 600) / 2,1000,600,TRUE
			'
		Case WM_COMMAND
			Select Case HiWord(wParam)
				Case BN_CLICKED,1
					Select Case LoWord(wParam)
						Case IDM_FILE_EXIT
							SendMessage(hWin,WM_CLOSE,0,0)
							'
						Case IDM_HELP_ABOUT
							ShellAbout(hWin,Cast(LPCTSTR,@AppName),Cast(LPCTSTR,@AboutMsg),NULL)
							'
					End Select
					'
			End Select
			'
		Case WM_SIZE
			'
		Case WM_CLOSE
			DestroyWindow(hWin)
			'
		Case WM_DESTROY
			PostQuitMessage(NULL)
			'
		Case Else
			Return DefWindowProc(hWin,uMsg,wParam,lParam)
			'
	End Select
	Return 0

End Function   
             
Function WinMain(ByVal hInst As HINSTANCE,ByVal hPrevInst As HINSTANCE,ByVal CmdLine As LPCTSTR,ByVal CmdShow As Integer) As LRESULT
	Dim wc As WNDCLASSEX
	Dim msg As MSG
	
	Print "函数"

	' Setup and register class for dialog
	wc.cbSize        = SizeOf(WNDCLASSEX)
	wc.style         = CS_HREDRAW or CS_VREDRAW
	wc.lpfnWndProc   = @WndProc
	wc.cbClsExtra    = 0
	wc.cbWndExtra    = DLGWINDOWEXTRA
	wc.hInstance     = hInst
	wc.hbrBackground = Cast(HBRUSH,COLOR_BTNFACE+1)
	wc.lpszMenuName  = Cast(LPCTSTR,IDM_MENU)
	wc.lpszClassName = Cast(LPCTSTR,@ClassName)
	wc.hIcon         = LoadIcon(NULL,IDI_APPLICATION)
	wc.hIconSm       = wc.hIcon
	wc.hCursor       = LoadCursor(NULL,IDC_ARROW)
	Print RegisterClassEx(@wc)
	' Create and show the dialog
	CreateDialogParam(hInst,MAKEINTRESOURCE(IDD_DIALOG),NULL,@WndProc,NULL)
	if hWndMain Then
		ShowWindow(hWndMain,SW_SHOWNORMAL)
		UpdateWindow(hWndMain)
		' Message loop
		Do While GetMessage(@msg,NULL,0,0)
			TranslateMessage(@msg)
			DispatchMessage(@msg)
		Loop
		Return msg.wParam
	Else
		Print "创建窗体错误",GetLastError()
		Sleep
		Return 0
	EndIf
End Function

'{ Program start
    InitCommonControls
    WinMain(GetModuleHandle(NULL),NULL,GetCommandLine,SW_SHOWDEFAULT)
'    
	ExitProcess(0)
    End
'}
'Program End
