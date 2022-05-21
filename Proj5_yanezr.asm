TITLE Display Random Numbers     (Proj5_yanezr.asm)

; Author: Ricardo Yanez
; Last Modified: 5/20/2022
; OSU email address: yanezr@oregonstate.edu
; Course number/section: CS271 Section 404
; Project Number: 5 Due Date: 5/22/2022
; Description: Generate random numbers within a range, sort, calculate
;	           median and count instances

INCLUDE Irvine32.inc

; define limits
LO = 15
HI = 50

ARRAYSIZE = 200

.data

  greeting		BYTE	"Generating, Sorting, and Counting Random integers! Programmed by Ricardo",13,10,10,0

  prompt1		BYTE	"This program generates ",0
  prompt2		BYTE	" random numbers in the range [",0
  prompt3		BYTE	"], displays the",13,10,0
  prompt4		BYTE	"original list, sorts the list, displays the median value of the list, displays the",13,10
				BYTE	"list sorted in ascending order, then displays the number of instances of each",13,10
				BYTE	"generated value, starting with the number of ",0
  prompt5		BYTE	"s.",13,10,10,0

  dash			BYTE	"-",0
  space			BYTE	" ",0

  randArray		DWORD	ARRAYSIZE DUP (?)
  counts		DWORD	HI-LO+1 DUP (0)

  title_1		BYTE	"Your unsorted random numbers:",13,10,0
  title_2		BYTE	"The median value of the array: ",0
  title_3		BYTE	"Your sorted random numbers:",13,10,0
  title_4		BYTE	"Your list of instances of each generated number, starting with the number of ",0
  title_5		BYTE	"s:",13,10,0

  goodbye		BYTE	"Goodbye, and thanks for using this program!",13,10,0

.code
main PROC

  CALL Randomize			; Initialize the seed

  PUSH OFFSET greeting
  PUSH OFFSET prompt1
  PUSH OFFSET prompt2
  PUSH OFFSET prompt3
  PUSH OFFSET prompt4
  PUSH OFFSET prompt5
  PUSH OFFSET dash
  CALL introduction			; Display greeting and introduction

  PUSH OFFSET randArray
  CALL fillArray			; Fill array with random mumbers

  ;------------------------------------
  ; print unsorted random numbers title
  ;------------------------------------
  MOV EDX, OFFSET title_1
  CALL WriteString

  PUSH ARRAYSIZE
  PUSH OFFSET randArray
  CALL displayList			; print unsorted array

  ;-------------------
  ; print median title
  ;-------------------
  MOV EDX, OFFSET title_2
  CALL WriteString

  PUSH OFFSET randArray
  CALL displayMedian		; Calculate median and print

  ;----------------------------------
  ; print sorted random numbers title
  ;----------------------------------
  MOV EDX, OFFSET title_3
  CALL WriteString

  PUSH OFFSET randArray
  CALL sortList				; Sort array

  PUSH ARRAYSIZE
  PUSH OFFSET randArray
  CALL displayList			; print sorted array

  ;-----------------------------
  ; print instance counter title
  ;-----------------------------
  MOV EDX, OFFSET title_4
  CALL WriteString
  MOV EAX, LO
  CALL WriteDec
  MOV EDX, OFFSET title_5
  CALL WriteString

  PUSH OFFSET randArray
  PUSH OFFSET counts
  CALL countList			; Count element instances of array

  PUSH HI-LO+1
  PUSH OFFSET counts
  CALL displayList			; print instance counter

  PUSH OFFSET goodbye
  CALL farewell				; Display end credits

  Invoke ExitProcess,0		; exit to operating system
main ENDP


;---------------------------------------------------;
; Name: introduction                                ;
;                                                   ;
; Display program title and instructions.           ;
;                                                   ;
; Preconditions: none                               ;
;                                                   ;
; Postconditions: none                              ;
;                                                   ;
; Receives: greeting and introduction strings       ;
;                                                   ;
; Returns: none                                     ;
;---------------------------------------------------;
  introduction PROC

    PUSH EBP
	MOV EBP, ESP

	;-----------------
	; display greeting
	;-----------------
	MOV EDX, [EBP+32]
	CALL WriteString

	;---------------------
	; display instructions
	;---------------------
	MOV EDX, [EBP+28]
	CALL WriteString

	MOV EAX, ARRAYSIZE
	CALL WriteDec

	MOV EDX, [EBP+24]
	CALL WriteString

	MOV EAX, LO
	CALL WriteDec

	MOV EDX, [EBP+8]
	CALL WriteString

	MOV EAX, HI
	CALL WriteDec

	MOV EDX, [EBP+20]
	CALL WriteString

	MOV EDX, [EBP+16]
	CALL WriteString

	MOV EAX, LO
	CALL WriteDec

	MOV EDX, [EBP+12]
	CALL WriteString

	POP EBP
	RET 28
  introduction ENDP

;-------------------------------------------------------;
; Name: fillArray                                       ;
;                                                       ;
; Fill array with random numbers in the range [LO,HI]   ;
;                                                       ;
; Preconditions: HI, LO, ARRAYSIZE constants            ;
;                                                       ;
; Postconditions: none                                  ;
;                                                       ;
; Receives: none                                        ;
;                                                       ;
; Returns: array                                        ;
;-------------------------------------------------------;
  fillArray PROC

    PUSH EBP
	MOV EBP, ESP

	MOV EDI, [EBP+8]		; address of array
	MOV ECX, ARRAYSIZE		; size of array

	;------------------------------------
	; loop and populate array with random
	; numbers in the range [LO,HI]
	;------------------------------------
_loop:
	MOV EAX, HI
	INC EAX
	CALL RandomRange
	CMP EAX, LO
	JL _loop
	MOV [EDI], EAX
	ADD EDI, SIZEOF DWORD
	LOOP _loop

	POP EBP
    RET 4
  fillArray ENDP


;-------------------------------------------------------;
; Name: displayList                                     ;
;                                                       ;
; Print array                                           ;
;                                                       ;
; Preconditions: none                                   ;
;                                                       ;
; Postconditions: none                                  ;
;                                                       ;
; Receives: size of array and array                     ;
;                                                       ;
; Returns: none                                         ;
;-------------------------------------------------------;
  displayList PROC

    PUSH EBP
	MOV EBP, ESP

	MOV ESI, [EBP+8]		; address of array
	MOV ECX, [EBP+12]		; size of array
	MOV EBX, 0				; counter

	;--------------------------------
	; loop and print an array element
	; followed by a space
	;--------------------------------
_loop:
	MOV EAX, [ESI]
	CALL WriteDec
	MOV AL, ' '
	CALL WriteChar
	ADD ESI, SIZEOF DWORD
	INC EBX
	PUSH EBX
	CALL printNewline		; print 20 numbers per line
	LOOP _loop

	;--------------------------------
	; print extra new line if counter
	; is not a multiple of 20
	;--------------------------------
	MOV EDX, 0
	MOV EAX, EBX
	MOV EBX, 20
	DIV EBX
	CMP EDX, 0
	JZ _skip
	CALL CrLf
_skip:
	CALL CrLf

	POP EBP
    RET 8
  displayList ENDP


;-------------------------------------------------------;
; Name: printNewline                                    ;
;                                                       ;
; Print new line after 20 numbers displayed             ;
;                                                       ;
; Preconditions: called from inside a loop              ;
;                                                       ;
; Postconditions: none                                  ;
;                                                       ;
; Receives: count                                       ;
;                                                       ;
; Returns: none                                         ;
;-------------------------------------------------------;
  printNewline PROC
	PUSH EBP
	MOV EBP, ESP

	PUSH EAX			; preserve registers
	PUSH EBX
	PUSH EDX

	;--------------------
	; print a new line if
	; count % 20 == 0
	;--------------------
	MOV EDX, 0
	MOV EAX, [EBP+8]	; count
	MOV EBX, 20
	DIV EBX
	CMP EDX, 0
	JNZ _end
	CALL CrLf
_end:

	POP EDX				; restore registers
	POP EBX
	POP EAX

	POP EBP
	RET 4
  printNewline ENDP


;-------------------------------------------------------;
; Name: displayMedian                                   ;
;                                                       ;
; Calculate and display median of array                 ;
;                                                       ;
; Preconditions: none                                   ;
;                                                       ;
; Postconditions: none                                  ;
;                                                       ;
; Receives: array                                       ;
;                                                       ;
; Returns: none                                         ;
;-------------------------------------------------------;
  displayMedian PROC

    PUSH EBP
	MOV EBP, ESP

	MOV ESI, [EBP+8]	; address of array
	MOV ECX, ARRAYSIZE	; size of array
	MOV EAX, 0			; reset accumulator

	;---------------
	; sum all values
	;---------------
_loop:
	ADD EAX, [ESI]
	ADD ESI, 4
	LOOP _loop
	;-----------------
	; calculate median
	;-----------------
	MOV EDX, 0
	MOV EBX, ARRAYSIZE
	DIV EBX
	MOV ECX, EAX		; save median

	; ------------------------------------------
	; Round up using the remainder. Multiply the
	; remainder by two. If it is larger than the
	; divisor, add one to the median.
	;-------------------------------------------
	MOV EAX, EDX
	MOV EDX, 0
	MOV EBX, 2
	MUL EBX
	CMP EAX, ARRAYSIZE
	JB _skip
	INC ECX				; round up
_skip:
	MOV EAX, ECX

	CALL WriteDec
	CALL CrLf
	CALL CrLf

	POP EBP
    RET 4
  displayMedian ENDP


;-------------------------------------------------------;
; Name: sortList                                        ;
;                                                       ;
; Sort array in ascending order and print               ;
;                                                       ;
; Preconditions: none                                   ;
;                                                       ;
; Postconditions: none                                  ;
;                                                       ;
; Receives: title, array                                ;
;                                                       ;
; Returns: none                                         ;
;-------------------------------------------------------;
  sortList PROC

    PUSH EBP
	MOV EBP, ESP

	;----------------------------------------------
	; use nested loops to compare and swap elements
	; extremey rudamentary sort
	;----------------------------------------------
	MOV ECX, ARRAYSIZE-1	; outer loop counter

_outer_loop:
	MOV ESI, [EBP+8]		; address of array
	MOV EBX, ECX			; inner loop counter

_inner_loop:
	MOV EAX, [ESI]
	ADD ESI, SIZEOF DWORD
	MOV EDX, [ESI]
	CMP EAX, EDX
	JBE _skip
	CALL exchangeElements
_skip:
	DEC EBX
	JNZ _inner_loop

	DEC ECX
	JNZ _outer_loop

	POP EBP
    RET 4
  sortList ENDP


;-------------------------------------------------------;
; Name: exchangeElements                                ;
;                                                       ;
; Swap two adjacent elements of an array                ;
;                                                       ;
; Preconditions: ESI must hold the address of the       ;
;                second element. EAX and EDX must hold  ;
;                the values to exchange                 ;
;                                                       ;
; Postconditions: values exchanged                      ;
;                                                       ;
; Receives: none                                        ;
;                                                       ;
; Returns: none                                         ;
;-------------------------------------------------------;
  exchangeElements PROC

	MOV [ESI], EAX
	MOV [ESI-4], EDX

	RET
  exchangeElements ENDP

;-------------------------------------------------------;
; Name: countList                                       ;
;                                                       ;
; Count the number of incidences of values in array     ;
;                                                       ;
; Preconditions: none                                   ;
;                                                       ;
; Postconditions: none                                  ;
;                                                       ;
; Receives: array                                       ;
;                                                       ;
; Returns: counter                                      ;
;-------------------------------------------------------;
  countList PROC

    PUSH EBP
	MOV EBP, ESP

	MOV ESI, [EBP+12]		; address of input array
	MOV ECX, ARRAYSIZE		; size of input array

	;--------------------------
	; loop over array and count
	; each instance of a value
	;--------------------------
_count_loop:
	MOV EDI, [EBP+8]		; address of output array
	MOV EAX, [ESI]
	SUB EAX, LO				; index of counter
	MOV EBX, TYPE DWORD		; multiply by TYPE
	MUL EBX
	MOV EBX, [EDI+EAX]		; load count
	INC EBX					; increment by one
	MOV [EDI+EAX], EBX		; store count
	ADD ESI, SIZEOF DWORD
	LOOP _count_loop

	POP EBP
    RET 4
  countList ENDP


;-------------------------------------------------------;
; Name: farewell                                        ;
;                                                       ;
; Display end credits                                   ;
;                                                       ;
; Preconditions: none                                   ;
;                                                       ;
; Postconditions: none                                  ;
;                                                       ;
; Receives: string                                      ;
;                                                       ;
; Returns: none                                         ;
;-------------------------------------------------------;
  farewell PROC

    PUSH EBP
	MOV EBP, ESP

	;--------------------
	; display end credits
	;--------------------
	MOV EDX, [EBP+8]
	CALL WriteString

	POP EBP
	RET 4
  farewell ENDP

END main
