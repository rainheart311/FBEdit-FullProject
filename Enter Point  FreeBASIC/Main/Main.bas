'注意，此工程必须用Console建立

Function Main(ByVal argc As Integer,ByVal argv As ZString Ptr Ptr ) As Integer      
    print "param";argc,**argv
    print "end"   
    Sleep     
	Return True    
End Function


End Main(__FB_ARGC__,__FB_ARGV__)   