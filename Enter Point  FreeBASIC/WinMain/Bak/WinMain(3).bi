Type APP_TYPE
	CompanyName As ZString * MAX_PATH      '???? 
	ProductName As ZString * MAX_PATH      '???? 
    ExeName     As ZString * MAX_PATH      'Exe?? 
    Path        As ZString * MAX_PATH      'Exe???? 
    hInstance   As HINSTANCE               ' 
    Major       As Long                    '????
    Minor       As Long                    '????  
    Revision    As Long                    '?????
    Build       As Long                    '?????
End Type

#Define IDD_DIALOG			1000

#Define IDM_MENU		    10000
#Define IDM_FILE_EXIT		10001
#Define IDM_HELP_ABOUT		10101

Dim Shared App As APP_TYPE
Dim Shared hWndMain As HWND

Const ClassName = "D"
Const AppName   = "Dialog as main"
Const AboutMsg  = !"FbEdit Dialog as main\13\10Copyright © FbEdit 2007"
