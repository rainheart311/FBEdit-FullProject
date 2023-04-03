#Include Once "windows.bi"

'===================================================================================================================
'方法1:没有线程启动和终止的参数
'使用Constructor和Destructor来判断加载和卸载调用，以此设置fdwReason参数,具体如下：
'获取当前模块句柄
'Function GetCurrentModuleHandle() As HMODULE
'    dim info As MEMORY_BASIC_INFORMATION
'    VirtualQuery(cast(LPCVOID,@GetCurrModuleHandle),@info,SizeOf(info))
'    Return cast(HMODULE,info.AllocationBase)
'End Function
 
'DLL入口函数 
'Function DllMain(ByVal hModule As HANDLE, ByVal fdwReason As Dword, ByVal lpReserved As LPVOID) As BOOL Export 
'    Select Case fdwReason
'    	Case DLL_PROCESS_ATTACH  'DLL被加载
'            MessageBox(0,"DLL_PROCESS_ATTACH","hModule=" & Hex(hModule),0)
'    	Case DLL_PROCESS_DETACH  'DLL被卸载
'            MessageBox(0,"DLL_PROCESS_DETACH","hModule=" & Hex(hModule),0)
'    End Select  
'    Function = TRUE
'End Function

'DLL启动时调用
'Sub LoadDllMain() Constructor
'	DllMain(GetCurrentModuleHandle(),DLL_PROCESS_ATTACH,NULL)
'End Sub

'DLL卸载时调用
'Sub UnloadDllMain() Destructor
'	DllMain(GetCurrentModuleHandle(),DLL_PROCESS_DETACH,NULL)
'End Sub

'===================================================================================================================
'方法2，使用-Wl编译命令来设置入口点(64位无法使用)
'编译命令如下：fbc -s gui -dll -export -Wl "--entry _DLLMAIN"，在VFB内增加部分为-Wl "--entry _DLLMAIN"
'Function DllMain(ByVal hModule As HANDLE, ByVal fwdReason As Dword, ByVal lpReserved As LPVOID) As BOOL Export 
'    Select Case fwdReason
'    	Case DLL_PROCESS_ATTACH  'DLL被加载
'            MessageBox(0,"DLL_PROCESS_ATTACH","hModule=" & Hex(hModule),0)
'    	Case DLL_PROCESS_DETACH  'DLL被卸载
'            MessageBox(0,"DLL_PROCESS_DETACH","hModule=" & Hex(hModule),0)
'    	Case DLL_THREAD_ATTACH   '单个线程启动
'            MessageBox(0,"DLL_THREAD_ATTACH","hModule=" & Hex(hModule),0)
'    	Case DLL_THREAD_DETACH   '单个线程终止
'    		MessageBox(0,"DLL_THREAD_DETACH","hModule=" & Hex(hModule),0)
'    	Case Else
'    End Select  
'    Function = TRUE
'End Function

'===================================================================================================================
'线程测试
Dim Shared pThread As HANDLE

Function TestThread(ByVal dat As Any Ptr) As DWORD       
    Print "hModule=";*Cast(HANDLE Ptr,dat)
	Print "in thread"
    Return 0                                                         
End Function   
'线程测试
'===================================================================================================================

'===================================================================================================================
'方法3，使用-Wl编译命令来设置入口点
'编译命令如下：fbc -s gui -dll -export -Wl -e_DLLENTRY，VFB中使用时，需要在工程熟悉窗口的附加编译选项添加：-Wl -e_DLLENTRY  
#cmdline "-Wl -e_DLLENTRY"   '添加入口命令
#Ifdef __FB_64BIT__
Function DllMain Alias "_DLLENTRY" (ByVal hModule As HANDLE, ByVal fdwReason As DWORD, ByVal lpReserved As LPVOID) As BOOL Export 
#Else
Function DllMain Alias "DLLENTRY" (ByVal hModule As HANDLE, ByVal fdwReason As DWORD, ByVal lpReserved As LPVOID) As BOOL Export 
#EndIf 
    Select Case fdwReason
    	Case DLL_PROCESS_ATTACH  'DLL被加载
            MessageBox(0,"DLL被加载","hModule=" & Hex(hModule),0)
'===================================================================================================================
'线程测试
            Dim dwID As DWORD 
            pThread = CreateThread(NULL,0,Cast(LPTHREAD_START_ROUTINE,@TestThread),@hModule,0,@dwID) '开启线程
'线程测试
'===================================================================================================================
    	Case DLL_PROCESS_DETACH  'DLL被卸载
            MessageBox(0,"DLL被卸载","hModule=" & Hex(hModule),0)
'===================================================================================================================
'线程测试
            CloseHandle(pThread) '关闭线程
            pThread = NULL 
'线程测试
'===================================================================================================================
    	Case DLL_THREAD_ATTACH   '单个线程启动
            MessageBox(0,"单个线程启动","hModule=" & Hex(hModule),0)
    	Case DLL_THREAD_DETACH   '单个线程终止
    		MessageBox(0,"单个线程终止","hModule=" & Hex(hModule),0)
    	Case Else
    End Select  
    Function = TRUE
End Function