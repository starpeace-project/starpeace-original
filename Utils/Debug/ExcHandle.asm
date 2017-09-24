	.386
	.MODEL	FLAT

	EXTRN	@HandleAnyException
	EXTRN	@HandleOnException
	EXTRN	@HandleAutoException

	PUBLIC  GetHandleAnyExceptionAddr
        PUBLIC  GetHandleOnExceptionAddr
	PUBLIC  GetHandleAutoExceptionAddr
	PUBLIC  GetExceptionHandler

	.CODE

GetHandleAnyExceptionAddr	PROC
	LEA	EAX,@HandleAnyException
	RET
GetHandleAnyExceptionAddr	ENDP

GetHandleOnExceptionAddr	PROC
	LEA	EAX,@HandleOnException
	RET
GetHandleOnExceptionAddr	ENDP

GetHandleAutoExceptionAddr	PROC
	LEA	EAX,@HandleAutoException
	RET
GetHandleAutoExceptionAddr	ENDP

GetExceptionHandler		PROC
	RET
GetExceptionHandler		ENDP


	END
