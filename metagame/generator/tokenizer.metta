;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


;
; 


  (= 
    (read-tokens-from-file $File $Tokens) 
    ( (see $File) 
      (read-tokens $Tokens) 
      (set-det) 
      (seen)))
; 

  (= 
    (read-tokens-from-file $File $Tokens) 
    ( (format "~nError: Couldn't read tokens from file ~w~n" 
        (:: $File)) 
      (seen) 
      (fail)))
; 



;
; 

;
; 

;
; 

;
; 


  (= 
    (read-tokens $Tokens) 
    ( (read-chars $Chars) (tokenize-chars $Chars $Tokens)))
; 


;
; 

;
; 

;
; 

;
; 


  (= 
    (read-keyboard-tokens $Tokens) 
    ( (read-chars-period-include $Chars) (tokenize-chars $Chars $Tokens)))
; 



;
; 

;
; 

;
; 


  (= 
    (read-chars $Chars) 
    (read-chars -1 n $Chars))
; 


;
; 

;
; 

;
; 


  (= 
    (read-line $Chars) 
    (read-chars 10 n $Chars))
; 



;
; 

;
; 

;
; 


  (= 
    (read-chars-period-include $Chars) 
    (read-chars 46 y $Chars))
; 


;
; 

;
; 

;
; 


  (= 
    (read-chars-period $Chars) 
    (read-chars 46 n $Chars))
; 



;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


  (= 
    (read-chars $End $Include $Chars) 
    (read-chars $End n n $Include $Chars))
; 


  (= 
    (read-chars $End $Quote $Comment $Include $Chars) 
    ( (read-char $C) (read-chars $C $End $Quote $Comment $Include $Chars)))
; 


  (= 
    (read-chars $End $End n n y 
      (:: $End)) 
    (set-det))
; 

  (= 
    (read-chars $End $End n n n Nil) 
    (set-det))
; 

  (= 
    (read-chars -1 $_ $_ $_ $_ Nil) 
    (set-det))
; 

  (= 
    (read-chars $C $End $Quote1 $Comment1 $Include 
      (Cons  $C $Cs)) 
    ( (toggle-contexts $C $Quote1 $Comment1 $Quote $Comment) (read-chars $End $Quote $Comment $Include $Cs)))
; 



  (= 
    (toggle-contexts $C $Quote1 $Comment1 $Quote $Comment) 
    ( (toggle-quote $C $Quote1 $Quote) (toggle-comment $C $Comment1 $Comment)))
; 


;
; 

;
; 


  (= 
    (toggle_quote  39 n y) True)
; 

  (= 
    (toggle_quote  39 y n) True)
; 

  (= 
    (toggle_quote  $_ $Q $Q) True)
; 


;
; 

;
; 

;
; 


  (= 
    (toggle_comment  37 n y) True)
; 

  (= 
    (toggle_comment  10 y n) True)
; 

  (= 
    (toggle_comment  $_ $C $C) True)
; 


;
; 


  (= 
    (read-char $Char) 
    (get0 $Char))
; 



;
; 

;
; 

;
; 

;
; 

;
; 


  (= 
    (read-tokens-from-string $String $Tokens) 
    (tokenize-chars $String $Tokens))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (tokenize-chars $Chars $Tokens) 
    (tokens $Tokens Nil $Chars $_))
; 



;
; 

;
; 



  (= 
    (-->  
      (tokens  
        (.) ()) 
      (46)) True)
; 

  (= 
    (-->  
      (tokens  $In $Out) 
      (,  
        (token  $In $S) 
        (tokens  $S $Out))) True)
; 



  (= 
    (-->  
      (token  
        (Cons  $C $Rest) $Rest) 
      (,  
        (identifier  $C) !)) True)
; 

  (= 
    (-->  
      (token  $Rest $Rest) 
      (,  
        (layout_string  $C) !)) True)
; 

  (= 
    (-->  
      (token  $Rest $Rest) 
      (,  
        (comment  $C) !)) True)
; 

  (= 
    (-->  
      (token  
        (Cons  $C $Rest) $Rest) 
      (operator  $C)) True)
; 

  (= 
    (-->  
      (token  
        (Cons  $C $Rest) $Rest) 
      (quote_symbol  $C)) True)
; 



;
; 

;
; 

;
; 


  (= 
    (-->  
      (quote_symbol  $C) 
      (,  
        (quote_char  $_) 
        (,  
          (non_quote_chars  $Cs) 
          (,  
            (quote_char  $_) 
            { (string_chars  $Cs $C) })))) True)
; 


  (= 
    (-->  
      (non_quote_chars  ()) ()) True)
; 

  (= 
    (-->  
      (non_quote_chars  
        (Cons  $C $Cs)) 
      (,  
        (non_quote_char  $C) 
        (more_non_quote_chars  $Cs))) True)
; 


  (= 
    (-->  
      (more_non_quote_chars  $Cs) 
      (non_quote_chars  $Cs)) True)
; 

  (= 
    (-->  
      (more_non_quote_chars  ()) ()) True)
; 


  (= 
    (-->  
      (non_quote_char  $C) 
      (,  
        (quote_char  $C) 
        (,  ! 
          {fail }))) True)
; 

  (= 
    (-->  
      (non_quote_char  $C) 
      ($C)) True)
; 


  (= 
    (-->  
      (quote_char  39) 
      (39)) True)
; 


;
; 

;
; 

;
; 



  (= 
    (-->  
      (comment  $C) 
      (,  
        (37) 
        (,  
          (non_lf_chars  $Cs) 
          (,  
            (linefeed  $_) 
            { (string_chars  $Cs $C) })))) True)
; 


  (= 
    (-->  
      (non_lf_chars  ()) ()) True)
; 

  (= 
    (-->  
      (non_lf_chars  
        (Cons  $C $Cs)) 
      (,  
        (non_lf_char  $C) 
        (more_non_lf_chars  $Cs))) True)
; 


  (= 
    (-->  
      (more_non_lf_chars  $Cs) 
      (non_lf_chars  $Cs)) True)
; 

  (= 
    (-->  
      (more_non_lf_chars  ()) ()) True)
; 


  (= 
    (-->  
      (non_lf_char  $C) 
      (,  
        (linefeed  $C) 
        (,  ! 
          {fail }))) True)
; 

  (= 
    (-->  
      (non_lf_char  $C) 
      ($C)) True)
; 


  (= 
    (-->  
      (linefeed  10) 
      (10)) True)
; 
 

;
; 

;
; 

;
; 



  (= 
    (-->  
      (operator  $X) 
      (,  
        (comparison_operator  $X) !)) True)
; 

  (= 
    (-->  
      (operator  $X) 
      (,  
        (transfer_operator  $X) !)) True)
; 

  (= 
    (-->  
      (operator  $X) 
      (,  
        ($Y) 
        { (terminal_char  $Y $X) })) True)
; 


  (= 
    (-->  
      (comparison_operator  >=) 
      (,  
        (62) 
        (61))) True)
; 

  (= 
    (-->  
      (comparison_operator  <=) 
      (,  
        (60) 
        (61))) True)
; 


  (= 
    (-->  
      (transfer_operator  ->) 
      (45 62)) True)
; 




  (= 
    (terminal-char $X $Y) 
    ( (terminal $X) (name $Y (:: $X))))
; 


;
; 


  (= 
    (terminal  123) True)
; 

  (= 
    (terminal  125) True)
; 

  (= 
    (terminal  44) True)
; 

  (= 
    (terminal  60) True)
; 

  (= 
    (terminal  62) True)
; 

  (= 
    (terminal  91) True)
; 

  (= 
    (terminal  93) True)
; 

  (= 
    (terminal  40) True)
; 

  (= 
    (terminal  41) True)
; 

  (= 
    (terminal  61) True)
; 


  (= 
    (terminal  45) True)
; 

  (= 
    (terminal  47) True)
; 

  (= 
    (terminal  59) True)
; 


;
; 

;
; 

;
; 



  (= 
    (-->  
      (layout_string  
        (Cons  $X $Xs)) 
      (,  
        (layout_char  $X) 
        (more_layout_chars  $Xs))) True)
; 

  (= 
    (-->  
      (more_layout_chars  $Xs) 
      (,  
        (layout_string  $Xs) !)) True)
; 

  (= 
    (-->  
      (more_layout_chars  ()) ()) True)
; 



  (= 
    (-->  
      (layout_char  $X) 
      (,  
        ($X) 
        { (layout_char  $X) })) True)
; 



  (= 
    (layout_char  9) True)
; 

  (= 
    (layout_char  10) True)
; 

  (= 
    (layout_char  32) True)
; 

	

;
; 

;
; 

;
; 



  (= 
    (-->  
      (identifier  $Id) 
      (,  
        (alphanumchars  $Chars) 
        { (string_chars  $Chars $Id) })) True)
; 




  (= 
    (string-chars $Chars $Id) 
    (name $Id $Chars))
; 



  (= 
    (chars_to_nums  () ()) True)
; 

  (= 
    (chars-to-nums 
      (Cons  $H $T) 
      (Cons  $NH $NT)) 
    ( (name $H 
        (:: $NH)) (chars-to-nums $T $NT)))
; 



  (= 
    (-->  
      (alphanumchars  
        (Cons  $X $Xs)) 
      (,  
        (alphanumchar  $X) 
        (more_alphanumchars  $Xs))) True)
; 


  (= 
    (-->  
      (more_alphanumchars  $X) 
      (,  
        (alphanumchars  $X) !)) True)
; 

  (= 
    (-->  
      (more_alphanumchars  ()) ()) True)
; 


  (= 
    (-->  
      (alphanumchar  $Y) 
      (,  
        ($X) 
        { (alphanumchar  $X $Y) })) True)
; 


;
; 


  (= 
    (alphanumchar $X $X) 
    ( (>= $X 97) (=< $X 122)))
; 

  (= 
    (alphanumchar $X $X) 
    ( (>= $X 48) (=< $X 57)))
; 

  (= 
    (alphanumchar $X $Y) 
    ( (>= $X 65) 
      (=< $X 90) 
      (downcase-char $X $Y)))
; 

  (= 
    (alphanumchar  95 95) True)
; 




  (= 
    (downcase-char $Upper $Lower) 
    (downcase-char-num $Upper $Lower))
; 



  (= 
    (downcase-char-num $Upper $Lower) 
    ( (uppercase $Upper) 
      (set-det) 
      (is $Lower 
        (- 
          (+ $Upper "a") "A"))))
; 

  (= 
    (downcase_char_num  $X $X) True)
; 



  (= 
    (uppercase $N) 
    ( (>= $N "A") (=< $N "Z")))
; 




;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 


;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (parsing-mode $M) 
    (parameter parsing-mode $M))
; 


;
; 


  (= 
    (set-parsing-mode $Mode) 
    (set-parameter parsing-mode $Mode))
; 


  (= 
    (set-parsing-mode) 
    (set-parsing-mode parsing))
; 


  (= 
    (set-printing-mode) 
    (set-parsing-mode printing))
; 


;
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 



  (= 
    (set-alpha-squares-mode $M) 
    (set-parameter alpha-squares-mode $M))
; 



  (= 
    (alpha-squares-mode $M) 
    (parameter alpha-squares-mode $M))
; 



;
; 

;
; 

;
; 

;
; 



  (= 
    (with-alpha-squares $Goal) 
    ( (alpha-squares-mode $Mode) 
      (set-alpha-squares-mode on) 
      (with-alpha-squares $Goal $Mode)))
; 


  (= 
    (with-alpha-squares $Goal $Mode) 
    ( (call $Goal) 
      (set-det) 
      (set-alpha-squares-mode $Mode)))
; 

  (= 
    (with-alpha-squares $Goal $Mode) 
    ( (set-alpha-squares-mode $Mode) (fail)))
; 



  (= 
    (recover-grammar) 
    (add-parameter alpha-squares-mode off))
; 


;
; 






  (= 
    (print-token line) 
    ( (set-det) (nl)))
; 

  (= 
    (print-token (tab $T)) 
    ( (set-det) (tab $T)))
; 

  (= 
    (print-token $X) 
    ( (write $X) (tab 1)))
; 



  (= 
    (print_tokens  ()) True)
; 

  (= 
    (print-tokens (Cons  $H $T)) 
    ( (print-token $H) (print-tokens $T)))
; 



  (= 
    (print-tokens-to-file $Tokens $File) 
    ( (set-printing-mode) 
      (tell $File) 
      (print-tokens $Tokens) 
      (told)))
; 




  (= 
    (print-token-to-string line Nil) 
    (set-det))
; 

  (= 
    (print-token-to-string 
      (tab $T) Nil) 
    (set-det))
; 

  (= 
    (print-token-to-string $X $String) 
    (name $X $String))
; 


;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 

;
; 




  (= 
    (print_tokens_to_string  () ()) True)
; 

  (= 
    (print-tokens-to-string 
      (Cons  $H $T) $String) 
    ( (print-token-to-string $H $StrH) 
      (print-tokens-to-string $T $StrT) 
      (append $StrH 
        (Cons  32 $StrT) $String)))
; 




