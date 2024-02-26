;                                           (convert-to-metta-file  tokenizer $_24244 metagame/generator/tokenizer.pl metagame/generator/tokenizer.metta)
;
; ============================================================
;
; METAGAME Game-Playing Workbench
;
; Copyright (c) 1992 Barney D. Pell
;
; ============================================================

;
; ;; tokenizer.pl
;
; ;;
;
; ;; Read from pascal-like syntax, into list.
;
; ;; Reading is CASE-INSENSITIVE: all alpha characters
;
; ;; are converted to lower case when read in.
;
; ;; Also ignores extra blanks, tabs, and linefeeds.
;
; ;; Comments occur from some point in a line started by ;,
;
; ;; and will be ignored to end of line.
;
; ;; Ignores spaces between operators and atoms,
;
; ;; so squares can be written (X,Y) instead of ( X , Y ).

;
; READ_TOKENS_FROM_FILE(+File,-Tokens)

  (= (read-tokens-from-file $File $Tokens)  
    ( (see $File) 
      (read-tokens $Tokens) 
      (set-det) seen))
  (= (read-tokens-from-file $File $Tokens)  
    ( (format "~nError: Couldn't read tokens from file ~w~n" 
        (:: $File)) seen fail))


;
; READ_TOKENS(-Tokens).
;
; First reads input literally (as number codes of characters),
;
; then converts to tokens.
;
; After this, the tokens can be parsed into a higher-level structure.

  (= (read-tokens $Tokens)  
    (read-chars $Chars)
    (tokenize-chars $Chars $Tokens))

;
; READ_KEYBOARD_TOKENS(-Tokens).
;
; First reads input literally (as number codes of characters),
;
; then converts to tokens.
;
; After this, the tokens can be parsed into a higher-level structure.

  (= (read-keyboard-tokens $Tokens)  
    (read-chars-period-include $Chars)
    (tokenize-chars $Chars $Tokens))


;
; READ_CHARS(-Chars)
;
; Reads in a list of numerical character codes, until encountering
;
; the end_of_file (returns code -1).

  (= (read-chars $Chars)  
    (read-chars -1 n $Chars))

;
; READ_LINE(-Chars)
;
; Reads in a list of numerical character codes, until encountering
;
; a new line (char code 10).

  (= (read-line $Chars)  
    (read-chars 10 n $Chars))


;
; READ_CHARS_PERIOD_INCLUDE(-Chars)
;
; Reads in a list of numerical character codes, until encountering
;
; a period, which will be the last char.

  (= (read-chars-period-include $Chars)  
    (read-chars 46 y $Chars))

;
; READ_CHARS_PERIOD(-Chars)
;
; Reads in a list of numerical character codes, until encountering
;
; a period, which will be omitted from the string.

  (= (read-chars-period $Chars)  
    (read-chars 46 n $Chars))


;
; READ_CHARS(+EndChar,+Include,-Chars)
;
; Reads in a list of numerical character codes, until encountering
;
; either:
;
; a. end_of_file [-1], or
;
; b. EndChar, unless in a quotation ('...') context.
;
; c. EndChar, unless in a comment (; ...) context.
;
; EndChar will be included in Chars if Include=y.
;
; 

  (= (read-chars $End $Include $Chars)  
    (read-chars $End n n $Include $Chars))

  (= (read-chars $End $Quote $Comment $Include $Chars)  
    (read-char $C)
    (read-chars $C $End $Quote $Comment $Include $Chars))

  (= (read-chars $End $End n n y (:: $End))  
    (set-det))
  (= (read-chars $End $End n n n Nil)  
    (set-det))
  (= (read-chars -1 $_ $_ $_ $_ Nil)  
    (set-det))
  (= (read-chars $C $End $Quote1 $Comment1 $Include (Cons  $C $Cs))  
    (toggle-contexts $C $Quote1 $Comment1 $Quote $Comment)
    (read-chars $End $Quote $Comment $Include $Cs))


  (= (toggle-contexts $C $Quote1 $Comment1 $Quote $Comment)  
    (toggle-quote $C $Quote1 $Quote)
    (toggle-comment $C $Comment1 $Comment))

;
; toggle_quote(Char,OldQuote,NewQuote)
;
; Quote char is: 0'' (ie.: "'").

  (= (toggle-quote  39 n y)  True)
  (= (toggle-quote  39 y n)  True)
  (= (toggle-quote  $_ $Q $Q)  True)

;
; toggle_comment(Char,OldComment,NewComment)
;
; Start Comment char is: 0'; (ie.: ";").
;
; End Comment char is newline.

  (= (toggle-comment  37 n y)  True)
  (= (toggle-comment  10 y n)  True)
  (= (toggle-comment  $_ $C $C)  True)

;
; read_char(Char) :- get0(N), name(Char,[N]).

  (= (read-char $Char)  
    (get0 $Char))


;
; READ_TOKENS_FROM_STRING(+String,-Tokens).
;
; Reads input from String (list of number codes of characters),
;
; then converts to tokens.
;
; After this, the tokens can be parsed into a higher-level structure.
;
; This is just an alias for the procedure below.

  (= (read-tokens-from-string $String $Tokens)  
    (tokenize-chars $String $Tokens))

;
; TOKENIZE_CHARS(+Chars,-Tokens)
;
; Given a string of characters (like "barney." or
;
; equivalently [0'b,0'a,0'r,0'n,0'e,0'y,0'.]
;
; or the corresponding numbers [98,97,114,110,101,121,46]
;
; Tokens is a list of atomic tokens read from this
;
; string.
;
; For the simple requirements of game and move grammars, this also
;
; gives us the ability to read directly from strings.
;
; Note that the tokens in the string ends when we read the 0'.
;
; character. If a string doesn't have this, we fail.


  (= (tokenize-chars $Chars $Tokens)  
    (tokens $Tokens Nil $Chars $_))


;
; tokenize_chars(Chars,Tokens) :-
;
; tokens(Tokens,[],Chars,[]).


  (= (-->  (tokens  (.) ()) (46))  True)
  (= (-->  (tokens  $In $Out) (,  (token  $In $S) (tokens  $S $Out)))  True)


  (= (-->  (token  ([|]  $C $Rest) $Rest) (,  (identifier  $C) !))  True)
  (= (-->  (token  $Rest $Rest) (,  (layout-string  $C) !))  True)
  (= (-->  (token  $Rest $Rest) (,  (comment  $C) !))  True)
  (= (-->  (token  ([|]  $C $Rest) $Rest) (operator  $C))  True)
  (= (-->  (token  ([|]  $C $Rest) $Rest) (quote-symbol  $C))  True)


;
; ----------------------------------------
;
; Quoted atom
;
; ----------------------------------------

  (= (-->  (quote-symbol  $C) (,  (quote-char  $_) (,  (non-quote-chars  $Cs) (,  (quote-char  $_) {(string-chars  $Cs $C) }))))  True)

  (= (-->  (non-quote-chars  ()) ())  True)
  (= (-->  (non-quote-chars  ([|]  $C $Cs)) (,  (non-quote-char  $C) (more-non-quote-chars  $Cs)))  True)

  (= (-->  (more-non-quote-chars  $Cs) (non-quote-chars  $Cs))  True)
  (= (-->  (more-non-quote-chars  ()) ())  True)

  (= (-->  (non-quote-char  $C) (,  (quote-char  $C) (,  ! {fail })))  True)
  (= (-->  (non-quote-char  $C) ($C))  True)

  (= (-->  (quote-char  39) (39))  True)

;
; ----------------------------------------
;
; Comment
;
; ----------------------------------------


  (= (-->  (comment  $C) (,  (37) (,  (non-lf-chars  $Cs) (,  (linefeed  $_) {(string-chars  $Cs $C) }))))  True)

  (= (-->  (non-lf-chars  ()) ())  True)
  (= (-->  (non-lf-chars  ([|]  $C $Cs)) (,  (non-lf-char  $C) (more-non-lf-chars  $Cs)))  True)

  (= (-->  (more-non-lf-chars  $Cs) (non-lf-chars  $Cs))  True)
  (= (-->  (more-non-lf-chars  ()) ())  True)

  (= (-->  (non-lf-char  $C) (,  (linefeed  $C) (,  ! {fail })))  True)
  (= (-->  (non-lf-char  $C) ($C))  True)

  (= (-->  (linefeed  10) (10))  True) 

;
; ----------------------------------------
;
; Operator
;
; ----------------------------------------


  (= (-->  (operator  $X) (,  (comparison-operator  $X) !))  True)
  (= (-->  (operator  $X) (,  (transfer-operator  $X) !))  True)
  (= (-->  (operator  $X) (,  ($Y) {(terminal-char  $Y $X) }))  True)

  (= (-->  (comparison-operator  >=) (,  (62) (61)))  True)
  (= (-->  (comparison-operator  <=) (,  (60) (61)))  True)

  (= (-->  (transfer-operator  ->) (45 62))  True)



  (= (terminal-char $X $Y)  
    (terminal $X)
    (name $Y 
      (:: $X)))

;
; terminal(0'.).

  (= (terminal  123)  True)
  (= (terminal  125)  True)
  (= (terminal  44)  True)
  (= (terminal  60)  True)
  (= (terminal  62)  True)
  (= (terminal  91)  True)
  (= (terminal  93)  True)
  (= (terminal  40)  True)
  (= (terminal  41)  True)
  (= (terminal  61)  True)

  (= (terminal  45)  True)
  (= (terminal  47)  True)
  (= (terminal  59)  True)

;
; ----------------------------------------
;
; Layout String
;
; ----------------------------------------


  (= (-->  (layout-string  ([|]  $X $Xs)) (,  (layout-char  $X) (more-layout-chars  $Xs)))  True)
  (= (-->  (more-layout-chars  $Xs) (,  (layout-string  $Xs) !))  True)
  (= (-->  (more-layout-chars  ()) ())  True)


  (= (-->  (layout-char  $X) (,  ($X) {(layout-char  $X) }))  True)


  (= (layout-char  9)  True)
  (= (layout-char  10)  True)
  (= (layout-char  32)  True)
	

;
; ----------------------------------------
;
; Identifier
;
; ----------------------------------------


  (= (-->  (identifier  $Id) (,  (alphanumchars  $Chars) {(string-chars  $Chars $Id) }))  True)



  (= (string-chars $Chars $Id)  
    (name $Id $Chars))


  (= (chars-to-nums  () ())  True)
; /* not used */
  (= (chars-to-nums (Cons  $H $T) (Cons  $NH $NT))  
    (name $H 
      (:: $NH))
    (chars-to-nums $T $NT))


  (= (-->  (alphanumchars  ([|]  $X $Xs)) (,  (alphanumchar  $X) (more-alphanumchars  $Xs)))  True)

  (= (-->  (more-alphanumchars  $X) (,  (alphanumchars  $X) !))  True)
  (= (-->  (more-alphanumchars  ()) ())  True)

  (= (-->  (alphanumchar  $Y) (,  ($X) {(alphanumchar  $X $Y) }))  True)

;
; converts to lower-case

  (= (alphanumchar $X $X)  
    (>= $X 97)
    (=< $X 122))
  (= (alphanumchar $X $X)  
    (>= $X 48)
    (=< $X 57))
  (= (alphanumchar $X $Y)  
    (>= $X 65)
    (=< $X 90)
    (downcase-char $X $Y))
  (= (alphanumchar  95 95)  True)



  (= (downcase-char $Upper $Lower)  
    (downcase-char-num $Upper $Lower))


  (= (downcase-char-num $Upper $Lower)  
    (uppercase $Upper)
    (set-det)
    (is $Lower 
      (- 
        (+ $Upper "a") "A")))
  (= (downcase-char-num  $X $X)  True)


  (= (uppercase $N)  
    (>= $N "A")
    (=< $N "Z"))



;
; ================================================================================
;
; Pretty-Printing Token Lists
;
; ================================================================================
;
; 
;
; When tokens are parsed, they can also be generated back with
;
; special printing information, such as insertion of lines and tabs.
;
; These routines print these out nicely, destroying the list notation.
;
; However, they can always be read back in again, by the tokenizer.

;
; ----------------------------------------
;
; PARSING_MODE(-Mode)
;
; Mode should be: PARSING in general,
;
; but PRINTING, when using grammars to generate strings which will
;
; then be pretty-printed.


  (= (parsing-mode $M)  
    (parameter parsing-mode $M))

;
; SET_PARSING_MODE(Mode)

  (= (set-parsing-mode $Mode)  
    (set-parameter parsing-mode $Mode))

  (= set-parsing-mode  
    (set-parsing-mode parsing))

  (= set-printing-mode  
    (set-parsing-mode printing))

;
; ----------------------------------------

;
; ALPHA_SQUARES_MODE(-Mode)
;
; ; Mode values: {on,off}
;
; ; description: whether can use (a,1) notation to denote
;
; ; squares in move and game grammars
;
; ; on: use (a,1) notation
;
; ; off: use (1,1) notation
;
; ; used in: grammar.pl, notation.pl, interface.pl, tokenizer.pl
;
; ; In general, this param is off, except for humans
;
; ; entering moves requiring completion. It could be
;
; ; used to generate pretty initial assignments for games,
;
; ; but CAUTION, it will not parse games or moves in the
;
; ; other mode.


  (= (set-alpha-squares-mode $M)  
    (set-parameter alpha-squares-mode $M))


  (= (alpha-squares-mode $M)  
    (parameter alpha-squares-mode $M))


;
; WITH_ALPHA_SQUARES(+Goal)
;
; Calls Goal with alpha squares mode on, then
;
; resets the mode to what it was before.
;
; Goal must be deterministic for this to work properly.


  (= (with-alpha-squares $Goal)  
    (alpha-squares-mode $Mode)
    (set-alpha-squares-mode on)
    (with-alpha-squares $Goal $Mode))

  (= (with-alpha-squares $Goal $Mode)  
    (call $Goal)
    (set-det)
    (set-alpha-squares-mode $Mode))
  (= (with-alpha-squares $Goal $Mode)  
    ( (set-alpha-squares-mode $Mode) fail))


  (= recover-grammar  
    (add-parameter alpha-squares-mode off))

;
; ----------------------------------------





  (= (print-token line)  
    ( (set-det) nl))
  (= (print-token (tab $T))  
    (set-det)
    (tab $T))
  (= (print-token $X)  
    (write $X)
    (tab 1))


  (= (print-tokens  ())  True)
  (= (print-tokens (Cons  $H $T))  
    (print-token $H)
    (print-tokens $T))


  (= (print-tokens-to-file $Tokens $File)  
    (set-printing-mode 
      (tell $File) 
      (print-tokens $Tokens) told))



  (= (print-token-to-string line Nil)  
    (set-det))
  (= (print-token-to-string (tab $T) Nil)  
    (set-det))
  (= (print-token-to-string $X $String)  
    (name $X $String))

;
; PRINT_TOKENS_TO_STRING(Tokens,String)
;
; Prints out Tokens (a list of atoms) into a String which.
;
; If String were then printed, the printout would
;
; look just like printing each of the tokens, with
;
; 1 space between each token.
;
; This string is of the right form to be read in later by
;
; READ_TOKENS_FROM_STRING/2 :
;
; 
;
; | ?- print_tokens_to_string([barney,is,happy,'.'],S), read_tokens_from_string(S,T).
;
; 
;
; S = [98,97,114,110,101,121,32,105,115,32,104,97,112,112,121,32,46,32],
;
; T = [barney,is,happy,'.'] ?



  (= (print-tokens-to-string  () ())  True)
  (= (print-tokens-to-string (Cons  $H $T) $String)  
    (print-token-to-string $H $StrH)
    (print-tokens-to-string $T $StrT)
    (append $StrH 
      (Cons  32 $StrT) $String))



