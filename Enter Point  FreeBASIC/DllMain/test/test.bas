'#Define UNICODE
#include once "windows.bi"
#Include Once "win/commctrl.bi"
#Include Once "win/commdlg.bi"
#Include Once "win/shellapi.bi"
#Include Once "test.bi"

Dim Shared hDll As Any Ptr

Function WndProc(ByVal hWin As HWND,ByVal uMsg As UINT,ByVal wParam As WPARAM,ByVal lParam As LPARAM) As Integer   
	Select Case uMsg
		Case WM_INITDIALOG
			hWndMain = hWin
			Dim rc As RECT
			GetWindowRect(hWin,@rc)
			Dim As Integer cx = GetSystemMetrics( SM_CXFULLSCREEN )   ' 
            Dim As Integer cy = GetSystemMetrics( SM_CYFULLSCREEN )   ' 
			MoveWindow hWin, (cx - rc.right + rc.left) / 2,(cy - rc.bottom + rc.top) / 2,rc.right - rc.left,rc.bottom - rc.top,TRUE
			'
		Case WM_COMMAND
			Select Case HiWord(wParam)
				Case BN_CLICKED,1
					Select Case LoWord(wParam)
						Case IDM_FILE_EXIT
							SendMessage(hWin,WM_CLOSE,0,0)
						Case IDM_HELP_ABOUT
							
						Case IDC_BUTTON1
							hDll = DylibLoad("test.dll")
							If hDll Then
								Print "加载成功"
							Else
								Print "加载失败"
							EndIf
						Case IDC_BUTTON2
							If hDll Then
								Print "准备卸载"
								DylibFree(hDll)
								hDll = 0
							Else
								Print "句柄为空"
							EndIf
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

Sub InitApp(hInst As HINSTANCE)
	Dim zTemp As ZString * MAX_PATH
    Dim x As Long
    
    App.CompanyName = "Write By RainHeart"
	App.ProductName = "MyFBEdit.exe"
	App.hInstance = hInst
	App.Major    = 1  'Version is 1.0.0.0
	App.Minor    = 0
	App.Revision = 0
	App.Build    = 0
	
	GetModuleFileName App.hInstance,zTemp,MAX_PATH
    x = InStrRev(zTemp, Any ":/\")
    If x Then 
        App.Path    =  Left(zTemp, x) 
        App.ExeName =  Mid(zTemp, x + 1)
    Else
        App.Path    =  "" 
        App.ExeName =  zTemp
    End If
	
	SetCurrentDirectory(@App.Path)
End Sub

Function WinMain(ByVal hInst As HINSTANCE,ByVal hPrevInst As HINSTANCE,ByVal CmdLine As LPCTSTR,ByVal CmdShow As Integer) As Integer
	Dim wc As WNDCLASSEX
	Dim msg As MSG
	
	InitApp(hInst)

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
	RegisterClassEx(@wc)
	' Create and show the dialog
	CreateDialogParam(hInst,MAKEINTRESOURCE(IDD_DIALOG),NULL,@WndProc,NULL)
	ShowWindow(hWndMain,SW_SHOWNORMAL)
	UpdateWindow(hWndMain)
	' Message loop
	Do While GetMessage(@msg,NULL,0,0)
		TranslateMessage(@msg)
		DispatchMessage(@msg)
	Loop
	Return msg.wParam

End Function

'{ Program start
    InitCommonControls
    WinMain(GetModuleHandle(NULL),NULL,GetCommandLine,SW_SHOWDEFAULT)
'    
	ExitProcess(0)
    End
'}
'Program End
