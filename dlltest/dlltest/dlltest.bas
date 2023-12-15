'1、如果需要给VB直接调用，则需要加上Extern "Windows-MS" ... End Extern
'   如果给FB使用，不需要加
'2、需要导出的函数，需要加Export，不加找不到入口点

#cmdline "-x ../vb_project/dlltest.dll" '直接将DLL生成到VB工程目录下，省的手动复制
										
Extern "Windows-MS"
#include Once "windows.bi"

'使用BSTR的时候需要声明
#include Once "win/wtypes.bi"
#include Once "win/shlobj.bi"
#include Once "win/oleauto.bi"

'1、数值
Function fAdd(ByVal a As Long,ByVal b As Long) As Long Export 
	Return a + b
End Function

Function fSub(ByVal a As Long,ByVal b As Long) As Long Export 
	If a < b Then Return -1
	Return a - b
End Function

'2、字符指针 相当于C的 char *
Sub fPrint(ByVal s As ZString Ptr) Export 
	MessageBox NULL,s,"显示",MB_OK
End Sub

#define ver WStr("1.0.0")
Function fVersion() As BSTR Export 
	Return SysAllocString(Cast(BSTR,@ver))
End Function

End Extern 