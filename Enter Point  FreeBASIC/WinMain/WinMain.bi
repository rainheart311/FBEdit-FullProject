#Define IDD_DIALOG			1000

#Define IDM_MENU		    10000
#Define IDM_FILE_EXIT		10001
#Define IDM_HELP_ABOUT		10101

Dim Shared hWndMain As HWND

#Ifndef UNICODE
Const ClassName = "Dialog"
Const AppName   = "Dialog as main"
Const AboutMsg  = !"FbEdit Dialog as main\13\10Copyright © FbEdit 2007"
#Else
Const ClassName = WStr("Dialog")
Const AppName   = WStr("Dialog as main")
Const AboutMsg  = WStr(!"FbEdit Dialog as main\13\10Copyright © FbEdit 2007")
#EndIf

