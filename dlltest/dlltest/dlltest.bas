'1�������Ҫ��VBֱ�ӵ��ã�����Ҫ����Extern "Windows-MS" ... End Extern
'   �����FBʹ�ã�����Ҫ��
'2����Ҫ�����ĺ�������Ҫ��Export�������Ҳ�����ڵ�

#cmdline "-x ../vb_project/dlltest.dll" 'ֱ�ӽ�DLL���ɵ�VB����Ŀ¼�£�ʡ���ֶ�����
										
Extern "Windows-MS"
#include Once "windows.bi"

'ʹ��BSTR��ʱ����Ҫ����
#include Once "win/wtypes.bi"
#include Once "win/shlobj.bi"
#include Once "win/oleauto.bi"

'1����ֵ
Function fAdd(ByVal a As Long,ByVal b As Long) As Long Export 
	Return a + b
End Function

Function fSub(ByVal a As Long,ByVal b As Long) As Long Export 
	If a < b Then Return -1
	Return a - b
End Function

'2���ַ�ָ�� �൱��C�� char *
Sub fPrint(ByVal s As ZString Ptr) Export 
	MessageBox NULL,s,"��ʾ",MB_OK
End Sub

#define ver WStr("1.0.0")
Function fVersion() As BSTR Export 
	Return SysAllocString(Cast(BSTR,@ver))
End Function

End Extern 