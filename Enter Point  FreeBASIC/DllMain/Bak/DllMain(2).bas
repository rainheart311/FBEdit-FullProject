#Include Once "windows.bi"

'===================================================================================================================
'����1:û���߳���������ֹ�Ĳ���
'ʹ��Constructor��Destructor���жϼ��غ�ж�ص��ã��Դ�����fdwReason����,�������£�
'��ȡ��ǰģ����
'Function GetCurrentModuleHandle() As HMODULE
'    dim info As MEMORY_BASIC_INFORMATION
'    VirtualQuery(cast(LPCVOID,@GetCurrModuleHandle),@info,SizeOf(info))
'    Return cast(HMODULE,info.AllocationBase)
'End Function
 
'DLL��ں��� 
'Function DllMain(ByVal hModule As HANDLE, ByVal fdwReason As Dword, ByVal lpReserved As LPVOID) As BOOL Export 
'    Select Case fdwReason
'    	Case DLL_PROCESS_ATTACH  'DLL������
'            MessageBox(0,"DLL_PROCESS_ATTACH","hModule=" & Hex(hModule),0)
'    	Case DLL_PROCESS_DETACH  'DLL��ж��
'            MessageBox(0,"DLL_PROCESS_DETACH","hModule=" & Hex(hModule),0)
'    End Select  
'    Function = TRUE
'End Function

'DLL����ʱ����
'Sub LoadDllMain() Constructor
'	DllMain(GetCurrentModuleHandle(),DLL_PROCESS_ATTACH,NULL)
'End Sub

'DLLж��ʱ����
'Sub UnloadDllMain() Destructor
'	DllMain(GetCurrentModuleHandle(),DLL_PROCESS_DETACH,NULL)
'End Sub

'===================================================================================================================
'����2��ʹ��-Wl����������������ڵ�(64λ�޷�ʹ��)
'�����������£�fbc -s gui -dll -export -Wl "--entry _DLLMAIN"����VFB�����Ӳ���Ϊ-Wl "--entry _DLLMAIN"
'Function DllMain(ByVal hModule As HANDLE, ByVal fwdReason As Dword, ByVal lpReserved As LPVOID) As BOOL Export 
'    Select Case fwdReason
'    	Case DLL_PROCESS_ATTACH  'DLL������
'            MessageBox(0,"DLL_PROCESS_ATTACH","hModule=" & Hex(hModule),0)
'    	Case DLL_PROCESS_DETACH  'DLL��ж��
'            MessageBox(0,"DLL_PROCESS_DETACH","hModule=" & Hex(hModule),0)
'    	Case DLL_THREAD_ATTACH   '�����߳�����
'            MessageBox(0,"DLL_THREAD_ATTACH","hModule=" & Hex(hModule),0)
'    	Case DLL_THREAD_DETACH   '�����߳���ֹ
'    		MessageBox(0,"DLL_THREAD_DETACH","hModule=" & Hex(hModule),0)
'    	Case Else
'    End Select  
'    Function = TRUE
'End Function

'===================================================================================================================
'�̲߳���
Dim Shared pThread As HANDLE

Function TestThread(ByVal dat As Any Ptr) As DWORD       
    Print "hModule=";*Cast(HANDLE Ptr,dat)
	Print "in thread"
    Return 0                                                         
End Function   
'�̲߳���
'===================================================================================================================

'===================================================================================================================
'����3��ʹ��-Wl����������������ڵ�
'�����������£�fbc -s gui -dll -export -Wl -e_DLLENTRY��VFB��ʹ��ʱ����Ҫ�ڹ�����Ϥ���ڵĸ��ӱ���ѡ����ӣ�-Wl -e_DLLENTRY  
#cmdline "-Wl -e_DLLENTRY"   '����������
#Ifdef __FB_64BIT__
Function DllMain Alias "_DLLENTRY" (ByVal hModule As HANDLE, ByVal fdwReason As DWORD, ByVal lpReserved As LPVOID) As BOOL Export 
#Else
Function DllMain Alias "DLLENTRY" (ByVal hModule As HANDLE, ByVal fdwReason As DWORD, ByVal lpReserved As LPVOID) As BOOL Export 
#EndIf 
    Select Case fdwReason
    	Case DLL_PROCESS_ATTACH  'DLL������
            MessageBox(0,"DLL������","hModule=" & Hex(hModule),0)
'===================================================================================================================
'�̲߳���
            Dim dwID As DWORD 
            pThread = CreateThread(NULL,0,Cast(LPTHREAD_START_ROUTINE,@TestThread),@hModule,0,@dwID) '�����߳�
'�̲߳���
'===================================================================================================================
    	Case DLL_PROCESS_DETACH  'DLL��ж��
            MessageBox(0,"DLL��ж��","hModule=" & Hex(hModule),0)
'===================================================================================================================
'�̲߳���
            CloseHandle(pThread) '�ر��߳�
            pThread = NULL 
'�̲߳���
'===================================================================================================================
    	Case DLL_THREAD_ATTACH   '�����߳�����
            MessageBox(0,"�����߳�����","hModule=" & Hex(hModule),0)
    	Case DLL_THREAD_DETACH   '�����߳���ֹ
    		MessageBox(0,"�����߳���ֹ","hModule=" & Hex(hModule),0)
    	Case Else
    End Select  
    Function = TRUE
End Function