Attribute VB_Name = "Module1"
Option Explicit

Declare Function Add Lib "dlltest" Alias "fAdd" (ByVal a As Long, ByVal b As Long) As Long
Declare Function Dec Lib "dlltest" Alias "fSub" (ByVal a As Long, ByVal b As Long) As Long
Declare Sub domsg Lib "dlltest" Alias "fPrint" (ByVal s As String)
Declare Function ver Lib "dlltest" Alias "fVersion" () As String


