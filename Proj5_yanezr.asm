TITLE Display Random Numbers     (Proj5_yanezr.asm)

; Author: Ricardo Yanez
; Last Modified: 5/20/2022
; OSU email address: yanezr@oregonstate.edu
; Course number/section: CS271 Section 404
; Project Number: 5 Due Date: 5/22/2022
; Description: Generate random numbers within a range, sort, calculate
;	           median and count

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

  title_1		BYTE	"Your unsorted random numbers:",13,10,0
  title_2		BYTE	"The median value of the array: ",0
  title_3		BYTE	"Your sorted random numbers:",13,10,0


  goodbye		BYTE	"Goodbye, and thanks for using this program!",13,10,0

.code
main PROC

  CALL Randomize           ; Initialize the seed

  PUSH OFFSET greeting
  PUSH OFFSET prompt1
  PUSH OFFSET prompt2
  PUSH OFFSET prompt3
  PUSH OFFSET prompt4
  PUSH OFFSET prompt5
  PUSH OFFSET dash
  CALL introduction		   ; Display greeting and instructions

  PUSH OFFSET randArray
  CALL fillArray           ; Fill array with random mumbers

  PUSH OFFSET title_2
  PUSH OFFSET randArray
  CALL displayMedian       ; Calculate and print median

  PUSH OFFSET goodbye
  CALL farewell			   ; Display end credits

  Invoke ExitProcess,0	   ; exit to operating system
main ENDP


;---------------------------------------------------;
; Name: introduction                                ;
;                                                   ;
; Display program title and instructions.           ;
;                                                   ;
; Preconditions: strings greeting, instruct,        ;
;                prompt_ec1 and prompt_ec2 declared ;
;                                                   ;
; Postconditions: none                              ;
;                                                   ;
; Receives: global variables greeting, instruct,    ;
;                  prompt_ec1 and prompt_ec2        ;
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
; Preconditions: HI, LO, ARRAYSIZE constants defined    ;
;                                                       ;
; Postconditions: none                                  ;
;                                                       ;
; Receives: none                                        ;
;                                                       ;
; Returns: randArr                                      ;
;-------------------------------------------------------;
  fillArray PROC

    PUSH EBP
	MOV EBP, ESP

	MOV EDI, [EBP+8]     ; address of array
	MOV ECX, ARRAYSIZE   ; size of array

_loop:
	MOV EAX, HI
	INC EAX
	CALL RandomRange
	CMP EAX, LO
	JL _loop
	MOV [EDI], EAX
	ADD EDI, 4
	LOOP _loop

	PUSH OFFSET title_1
	PUSH OFFSET space
	PUSH OFFSET randArray
	CALL displayList        ; print array with random mumbers
	CALL CrLf

	POP EBP
    RET 4
  fillArray ENDP


;-------------------------------------------------------;
; Name: displayList                                      ;
;                                                       ;
; Print array                                           ;
;                                                       ;
; Preconditions: none                                   ;
;                                                       ;
; Postconditions: none                                  ;
;                                                       ;
; Receives: title, space, randArr                       ;
;                                                       ;
; Returns: none                                         ;
;-------------------------------------------------------;
  displayList PROC

    PUSH EBP
	MOV EBP, ESP

	MOV EDX, [EBP+16]    ; print title
	CALL WriteString

	MOV EDX, [EBP+12]    ; blank space
	MOV ESI, [EBP+8]     ; address of array
	MOV ECX, ARRAYSIZE   ; size of array

_loop:
	MOV EAX, [ESI]
	CALL WriteDec
	CALL WriteString
	ADD ESI, 4
	CALL printNewline    ; print 20 numbers per line
	LOOP _loop

	POP EBP
    RET 12
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
; Receives: ECX loop counter                            ;
;                                                       ;
; Returns: none                                         ;
;-------------------------------------------------------;
  printNewline PROC
	PUSH EAX      ; preserve registers
	PUSH EDX

	MOV EDX, 0
	MOV EAX, ECX
	DEC EAX
	MOV EBX, 20
	DIV EBX
	CMP EDX, 0
	JG _end
	CALL CrLf
_end:

	POP EDX       ; restore registers
	POP EAX

	RET
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
; Receives: title, randArr                              ;
;                                                       ;
; Returns: none                                         ;
;-------------------------------------------------------;
  displayMedian PROC

    PUSH EBP
	MOV EBP, ESP

	MOV ESI, [EBP+8]     ; address of array
	MOV ECX, ARRAYSIZE   ; size of array
	MOV EAX, 0           ; reset accumulator

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
	MOV ECX, EAX         ; save median

	; -------------------------------------------------
	; Round using the remainder. Multiply the remainder
	; by two. If it is larger than the divisor, add one
	; to the average.
	;--------------------------------------------------
	MOV EAX, EDX
	MOV EDX, 0
	MOV EBX, 2
	MUL EBX
	CMP EAX, ARRAYSIZE
	JB _done
	INC ECX              ; round up
_done:
	MOV EAX, ECX

	;---------------
	; display median
	;---------------
	MOV EDX, [EBP+12]
	CALL WriteString
	CALL WriteDec
	CALL CrLf

	POP EBP
    RET 8
  displayMedian ENDP



;-------------------------------------------------------;
; Name: farewell                                        ;
;                                                       ;
; Display end credits                                   ;
;                                                       ;
; Preconditions: none                                   ;
;                                                       ;
; Postconditions: none                                  ;
;                                                       ;
; Receives: prompt3 string global variable              ;
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
