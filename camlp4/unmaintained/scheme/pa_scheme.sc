; pa_r.cmo pa_rp.cmo pa_extend.cmo q_MLast.cmo pr_dump.cmo
; **********************************************************************
;                                                                       
;                                Camlp4                                 
;                                                                       
;     Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt          
;                                                                       
;   Copyright 2002 Institut National de Recherche en Informatique et    
;   en Automatique.  All rights reserved.  This file is distributed     
;   under the terms of the GNU Library General Public License, with     
;   the special exception on linking described in file                  
;    ../../../LICENSE.                                                  
;                                                                       
; **********************************************************************
; $Id$

(open Pcaml)
(open Stdpp)

(type (choice 'a 'b) (sum (Left 'a) (Right 'b)))

; Buffer

(module Buff
 (struct
  (define buff (ref (String.create 80)))
  (define (store len x)
    (if (>= len (String.length buff.val))
        (:= buff.val (^ buff.val (String.create (String.length buff.val)))))
    (:= buff.val.[len] x)
    (succ len))
  (define (get len) (String.sub buff.val 0 len))))

; Lexer

(definerec skip_to_eol
  (parser
   (((` (or '\n' '\r'))) ())
   (((` _) s) (skip_to_eol s))))

(define no_ident ['(' ')' '[' ']' '{' '}' ' ' '\t' '\n' '\r' ';'])

(definerec (ident len)
  (parser
   (((` '.')) (values (Buff.get len) True))
   (((` x (not (List.mem x no_ident))) s) (ident (Buff.store len x) s))
   (() (values (Buff.get len) False))))

(define (identifier kwt (values s dot))
  (let ((con
          (try (begin (: (Hashtbl.find kwt s) unit) "")
           (Not_found
            (match s.[0]
             ((range 'A' 'Z') (if dot "UIDENTDOT" "UIDENT"))
             (_ (if dot "LIDENTDOT" "LIDENT")))))))
     (values con s)))

(definerec (string len)
  (parser
   (((` '"')) (Buff.get len))
   (((` '\\') (` c) s) (string (Buff.store (Buff.store len '\\') c) s))
   (((` x) s) (string (Buff.store len x) s))))

(definerec (end_exponent_part_under len)
  (parser
   (((` (as (range '0' '9') c)) s)
    (end_exponent_part_under (Buff.store len c) s))
   (() (values "FLOAT" (Buff.get len)))))

(define (end_exponent_part len)
  (parser
   (((` (as (range '0' '9') c)) s)
    (end_exponent_part_under (Buff.store len c) s))
   (() (raise (Stream.Error "ill-formed floating-point constant")))))

(define (exponent_part len)
  (parser
   (((` (as (or '+' '-') c)) s) (end_exponent_part (Buff.store len c) s))
   (((a (end_exponent_part len))) a)))

(definerec (decimal_part len)
  (parser
   (((` (as (range '0' '9') c)) s) (decimal_part (Buff.store len c) s))
   (((` (or 'e' 'E')) s) (exponent_part (Buff.store len 'E') s))
   (() (values "FLOAT" (Buff.get len)))))

(definerec (number len)
  (parser
   (((` (as (range '0' '9') c)) s) (number (Buff.store len c) s))
   (((` '.') s) (decimal_part (Buff.store len '.') s))
   (((` (or 'e' 'E')) s) (exponent_part (Buff.store len 'E') s))
   (() (values "INT" (Buff.get len)))))

(define binary
  (parser
   (((` (as (range '0' '1') c))) c)))

(define octal
  (parser
   (((` (as (range '0' '7') c))) c)))

(define hexa
  (parser
   (((` (as (or (range '0' '9') (range 'a' 'f') (range 'A' 'F')) c))) c)))

(definerec (digits_under kind len)
  (parser
   (((d kind) s) (digits_under kind (Buff.store len d) s))
   (() (Buff.get len))))

(define (digits kind bp len)
  (parser
   (((d kind) s) (values "INT" (digits_under kind (Buff.store len d) s)))
   ((s) ep
    (raise_with_loc (values
                       (Reloc.shift_pos bp Reloc.zero_loc)
                       (Reloc.shift_pos ep Reloc.zero_loc))
       (Failure "ill-formed integer constant")))))

(define (base_number kwt bp len)
  (parser
   (((` (or 'b' 'B')) s) (digits binary bp (Buff.store len 'b') s))
   (((` (or 'o' 'O')) s) (digits octal bp (Buff.store len 'o') s))
   (((` (or 'x' 'X')) s) (digits hexa bp (Buff.store len 'x') s))
   (((id (ident (Buff.store 0 '#')))) (identifier kwt id))))

(definerec (operator len)
  (parser
   (((` '.')) (Buff.get (Buff.store len '.')))
   (() (Buff.get len))))

(define (char_or_quote_id x)
  (parser
   (((` ''')) (values "CHAR" (String.make 1 x)))
   ((s) ep
    (if (List.mem x no_ident)
        (Stdpp.raise_with_loc (values
                                 (Reloc.shift_pos (- ep 2) Reloc.zero_loc)
                                 (Reloc.shift_pos (- ep 1) Reloc.zero_loc))
         (Stream.Error "bad quote"))
        (let* ((len (Buff.store (Buff.store 0 ''') x))
               ((values s dot) (ident len s)))
          (values (if dot "LIDENTDOT" "LIDENT") s))))))

(definerec (char len)
  (parser
   (((` ''')) len)
   (((` x) s) (char (Buff.store len x) s))))

(define quote
  (parser
   (((` '\\') (len (char (Buff.store 0 '\\'))))
    (values "CHAR" (Buff.get len)))
   (((` x) s) (char_or_quote_id x s))))

; The system with LIDENTDOT and UIDENTDOT is not great (it would be
; better to have a token DOT (actually SPACEDOT and DOT)) but it is
; the only way (that I have found) to have a good behaviour in the
; toplevel (not expecting tokens after a phrase). Drawbacks: 1/ to be
; complete, we should have STRINGDOT, RIGHTPARENDOT, and so on 2/ the
; parser rule with dot is right associative and we have to reverse
; the resulting tree (using the function leftify).
; This is a complicated issue: the behaviour of the OCaml toplevel
; is strange, anyway. For example, even without Camlp4, The OCaml
; toplevel accepts that:
;     # let x = 32;; foo bar match let )

(definerec*
  ((lexer kwt)
     (parser
      (((t (lexer0 kwt))
       (_ no_dot)) t)))
  (no_dot
    (parser
     (((` '.')) ep
      (Stdpp.raise_with_loc (values
                               (Reloc.shift_pos (- ep 1) Reloc.zero_loc)
                               (Reloc.shift_pos ep Reloc.zero_loc))
         (Stream.Error "bad dot")))
     (() ())))
  ((lexer0 kwt)
    (parser bp
     (((` (or '\t' '\n' '\r')) s) (lexer0 kwt s))
     (((` ' ') s) (after_space kwt s))
     (((` ';') (_ skip_to_eol) s) (lexer kwt s))
     (((` '(')) (values (values "" "(") (values bp (+ bp 1))))
     (((` ')') s) ep (values (values "" (rparen s)) (values bp ep)))
     (((` '[')) (values (values "" "[") (values bp (+ bp 1))))
     (((` ']')) (values (values "" "]") (values bp (+ bp 1))))
     (((` '{')) (values (values "" "{") (values bp (+ bp 1))))
     (((` '}')) (values (values "" "}") (values bp (+ bp 1))))
     (((` '"') (s (string 0))) ep
      (values (values "STRING" s) (values bp ep)))
     (((` ''') (tok quote)) ep (values tok (values bp ep)))
     (((` '<') (tok (less kwt))) ep (values tok (values bp ep)))
     (((` '-') (tok (minus kwt))) ep (values tok (values bp ep)))
     (((` '~') (tok tilde)) ep (values tok (values bp ep)))
     (((` '?') (tok question)) ep (values tok (values bp ep)))
     (((` '#') (tok (base_number kwt bp (Buff.store 0 '0')))) ep
      (values tok (values bp ep)))
     (((` (as (range '0' '9') c)) (tok (number (Buff.store 0 c)))) ep
      (values tok (values bp ep)))
     (((` (as (or '+' '*' '/') c)) (id (operator (Buff.store 0 c)))) ep
      (values (identifier kwt (values id False)) (values bp ep)))
     (((` x) (id (ident (Buff.store 0 x)))) ep
      (values (identifier kwt id) (values bp ep)))
     (() (values (values "EOI" "") (values bp (+ bp 1))))))
  (rparen
   (parser
    (((` '.')) ").")
    ((_) ")")))
  ((after_space kwt)
    (parser
     (((` '.')) ep (values (values "" ".") (values (- ep 1) ep)))
     (((x (lexer0 kwt))) x)))
  (tilde
    (parser
     (((` (as (range 'a' 'z') c)) ((values s dot) (ident (Buff.store 0 c))))
      (values "TILDEIDENT" s))
     (() (values "LIDENT" "~"))))
  (question
    (parser
     (((` (as (range 'a' 'z') c)) ((values s dot) (ident (Buff.store 0 c))))
      (values "QUESTIONIDENT" s))
     (() (values "LIDENT" "?"))))
  ((minus kwt)
    (parser
     (((` '.')) (identifier kwt (values "-." False)))
     (((` (as (range '0' '9') c))
      (n (number (Buff.store (Buff.store 0 '-') c)))) ep n)
     (((id (ident (Buff.store 0 '-')))) (identifier kwt id))))
  ((less kwt)
    (parser
     (((` ':') (lab (label 0)) (? (` '<') "'<' expected") (q (quotation 0)))
      (values "QUOT" (^ lab ":" q)))
     (((id (ident (Buff.store 0 '<')))) (identifier kwt id))))
  ((label len)
    (parser
     (((` (as (or (range 'a' 'z') (range 'A' 'Z') '_') c)) s)
      (label (Buff.store len c) s))
     (() (Buff.get len))))
  ((quotation len)
    (parser
     (((` '>') s) (quotation_greater len s))
     (((` x) s) (quotation (Buff.store len x) s))
     (() (failwith "quotation not terminated"))))
  ((quotation_greater len)
    (parser
     (((` '>')) (Buff.get len))
     (((a (quotation (Buff.store len '>')))) a))))

(define (lexer_using kwt (values con prm))
  (match con
   ((or "CHAR" "EOI" "INT" "FLOAT" "LIDENT" "LIDENTDOT" "QUESTIONIDENT"
     "QUOT" "STRING" "TILDEIDENT" "UIDENT" "UIDENTDOT")
    ())
   ("ANTIQUOT" ())
   ("" (try (Hashtbl.find kwt prm) (Not_found (Hashtbl.add kwt prm ()))))
   (_
    (raise
     (Token.Error
      (^ "the constructor \"" con "\" is not recognized by Plexer"))))))

(define (lexer_text (values con prm))
  (cond
   ((= con "") (^ "'"prm "'"))
   ((= prm "") con)
   (else (^ con " \"" prm "\""))))

(define (lexer_gmake ())
  (let ((kwt (Hashtbl.create 89)))
     {(Token.tok_func
       (Token.lexer_func_of_parser
        (lambda (s)
          (let (((values r (values bp ep)) (lexer kwt s)))
            (values r (values (Reloc.shift_pos bp Reloc.zero_loc)
                              (Reloc.shift_pos ep Reloc.zero_loc)))))))
      (Token.tok_using (lexer_using kwt))
      (Token.tok_removing (lambda))
      (Token.tok_match Token.default_match)
      (Token.tok_text lexer_text)
      (Token.tok_comm None)}))

; Building AST

(type sexpr
  (sum
   (Sacc MLast.loc sexpr sexpr)
   (Schar MLast.loc string)
   (Sexpr MLast.loc (list sexpr))
   (Sint MLast.loc string)
   (Sfloat MLast.loc string)
   (Slid MLast.loc string)
   (Slist MLast.loc (list sexpr))
   (Sqid MLast.loc string)
   (Squot MLast.loc string string)
   (Srec MLast.loc (list sexpr))
   (Sstring MLast.loc string)
   (Stid MLast.loc string)
   (Suid MLast.loc string)))

(define loc_of_sexpr
  (lambda_match
   ((or (Sacc loc _ _) (Schar loc _) (Sexpr loc _) (Sint loc _)
     (Sfloat loc _) (Slid loc _) (Slist loc _) (Sqid loc _) (Squot loc _ _)
     (Srec loc _) (Sstring loc _) (Stid loc _) (Suid loc _))
    loc)))
(define (error_loc loc err)
  (raise_with_loc loc (Stream.Error (^ err " expected"))))
(define (error se err) (error_loc (loc_of_sexpr se) err))

(define strm_n "strm__")
(define (peek_fun loc) <:expr< Stream.peek >>)
(define (junk_fun loc) <:expr< Stream.junk >>)

(define assoc_left_parsed_op_list ["+" "*" "+." "*." "land" "lor" "lxor"])
(define assoc_right_parsed_op_list ["and" "or" "^" "@"])
(define and_by_couple_op_list ["=" "<>" "<" ">" "<=" ">=" "==" "!="])

(define (op_apply loc e1 e2)
  (lambda_match
   ("and" <:expr< $e1$ && $e2$ >>)
   ("or" <:expr< $e1$ || $e2$ >>)
   (x <:expr< $lid:x$ $e1$ $e2$ >>)))

(define string_se
  (lambda_match
     ((Sstring loc s) s)
     (se (error se "string"))))

(define mod_ident_se
  (lambda_match
   ((Suid _ s) [(Pcaml.rename_id.val s)])
   ((Slid _ s) [(Pcaml.rename_id.val s)])
   (se (error se "mod_ident"))))

(define (lident_expr loc s)
  (if (&& (> (String.length s) 1) (= s.[0] '`'))
     (let ((s (String.sub s 1 (- (String.length s) 1))))
        <:expr< ` $s$ >>)
     <:expr< $lid:(Pcaml.rename_id.val s)$ >>))

(definerec*
  (module_expr_se
    (lambda_match
     ((Sexpr loc [(Slid _ "functor") (Suid _ s) se1 se2])
      (let* ((s (Pcaml.rename_id.val s))
             (mt (module_type_se se1))
             (me (module_expr_se se2)))
         <:module_expr< functor ($s$ : $mt$) -> $me$ >>))
     ((Sexpr loc [(Slid _ "struct") . sl])
      (let ((mel (List.map str_item_se sl)))
         <:module_expr< struct $list:mel$ end >>))
     ((Sexpr loc [se1 se2])
      (let* ((me1 (module_expr_se se1))
             (me2 (module_expr_se se2)))
         <:module_expr< $me1$ $me2$ >>))
     ((Suid loc s) <:module_expr< $uid:(Pcaml.rename_id.val s)$ >>)
     (se (error se "module expr"))))
  (module_type_se
    (lambda_match
     ((Sexpr loc [(Slid _ "functor") (Suid _ s) se1 se2])
      (let* ((s (Pcaml.rename_id.val s))
             (mt1 (module_type_se se1))
             (mt2 (module_type_se se2)))
         <:module_type< functor ($s$ : $mt1$) -> $mt2$ >>))
     ((Sexpr loc [(Slid _ "sig") . sel])
      (let ((sil (List.map sig_item_se sel)))
         <:module_type< sig $list:sil$ end >>))
     ((Sexpr loc [(Slid _ "with") se (Sexpr _ sel)])
      (let* ((mt (module_type_se se))
             (wcl (List.map with_constr_se sel)))
         <:module_type< $mt$ with $list:wcl$ >>))
     ((Suid loc s) <:module_type< $uid:(Pcaml.rename_id.val s)$ >>)
     (se (error se "module type"))))
  (with_constr_se
    (lambda_match
     ((Sexpr loc [(Slid _ "type") se1 se2])
      (let* ((tn (mod_ident_se se1))
             (te (ctyp_se se2)))
         (MLast.WcTyp loc tn [] te)))
     (se (error se "with constr"))))
  (sig_item_se
    (lambda_match
     ((Sexpr loc [(Slid _ "type") . sel])
      (let ((tdl (type_declaration_list_se sel)))
         <:sig_item< type $list:tdl$ >>))
     ((Sexpr loc [(Slid _ "exception") (Suid _ c) . sel])
      (let* ((c (Pcaml.rename_id.val c))
             (tl (List.map ctyp_se sel)))
         <:sig_item< exception $c$ of $list:tl$ >>))
     ((Sexpr loc [(Slid _ "value") (Slid _ s) se])
      (let* ((s (Pcaml.rename_id.val s))
             (t (ctyp_se se)))
         <:sig_item< value $s$ : $t$ >>))
     ((Sexpr loc [(Slid _ "external") (Slid _ i) se . sel])
      (let* ((i (Pcaml.rename_id.val i))
             (pd (List.map string_se sel))
             (t (ctyp_se se)))
         <:sig_item< external $i$ : $t$ = $list:pd$ >>))
     ((Sexpr loc [(Slid _ "module") (Suid _ s) se])
      (let* ((s (Pcaml.rename_id.val s))
             (mb (module_type_se se)))
         <:sig_item< module $s$ : $mb$ >>))
     ((Sexpr loc [(Slid _ "moduletype") (Suid _ s) se])
      (let* ((s (Pcaml.rename_id.val s))
             (mt (module_type_se se)))
         <:sig_item< module type $s$ = $mt$ >>))
     (se (error se "sig item"))))
  ((str_item_se se)
    (match se
     ((Sexpr loc [(Slid _ "open") se])
      (let ((s (mod_ident_se se))) <:str_item< open $s$ >>))
     ((Sexpr loc [(Slid _ "type") . sel])
      (let ((tdl (type_declaration_list_se sel)))
         <:str_item< type $list:tdl$ >>))
     ((Sexpr loc [(Slid _ "exception") (Suid _ c) . sel])
      (let* ((c (Pcaml.rename_id.val c))
             (tl (List.map ctyp_se sel)))
         <:str_item< exception $c$ of $list:tl$ >>))
     ((Sexpr loc [(Slid _ (as (or "define" "definerec") r)) se . sel])
      (let* ((r (= r "definerec"))
             ((values p e) (fun_binding_se se (begin_se loc sel))))
         <:str_item< value $opt:r$ $p$ = $e$ >>))
     ((Sexpr loc [(Slid _ (as (or "define*" "definerec*") r)) . sel])
      (let* ((r (= r "definerec*"))
             (lbs (List.map let_binding_se sel)))
         <:str_item< value $opt:r$ $list:lbs$ >>))
     ((Sexpr loc [(Slid _ "external") (Slid _ i) se . sel])
      (let* ((i (Pcaml.rename_id.val i))
             (pd (List.map string_se sel))
             (t (ctyp_se se)))
         <:str_item< external $i$ : $t$ = $list:pd$ >>))
     ((Sexpr loc [(Slid _ "module") (Suid _ i) se])
      (let* ((i (Pcaml.rename_id.val i))
             (mb (module_binding_se se)))
         <:str_item< module $i$ = $mb$ >>))
     ((Sexpr loc [(Slid _ "moduletype") (Suid _ s) se])
      (let* ((s (Pcaml.rename_id.val s))
             (mt (module_type_se se)))
         <:str_item< module type $s$ = $mt$ >>))
     (_
      (let* ((loc (loc_of_sexpr se))
             (e (expr_se se)))
         <:str_item< $exp:e$ >>))))
  ((module_binding_se se) (module_expr_se se))
  (expr_se
    (lambda_match
     ((Sacc loc se1 se2)
      (let ((e1 (expr_se se1)))
         (match se2
          ((Slist loc [se2])
           (let ((e2 (expr_se se2))) <:expr< $e1$ .[ $e2$ ] >>))
          ((Sexpr loc [se2])
           (let ((e2 (expr_se se2))) <:expr< $e1$ .( $e2$ ) >>))
          (_ (let ((e2 (expr_se se2))) <:expr< $e1$ . $e2$ >>)))))
     ((Slid loc s) (lident_expr loc s))
     ((Suid loc s) <:expr< $uid:(Pcaml.rename_id.val s)$ >>)
     ((Sint loc s) <:expr< $int:s$ >>)
     ((Sfloat loc s) <:expr< $flo:s$ >>)
     ((Schar loc s) <:expr< $chr:s$ >>)
     ((Sstring loc s) <:expr< $str:s$ >>)
     ((Stid loc s) <:expr< ~ $(Pcaml.rename_id.val s)$ >>)
     ((Sqid loc s) <:expr< ? $(Pcaml.rename_id.val s)$ >>)
     ((Sexpr loc []) <:expr< () >>)
     ((when (Sexpr loc [(Slid _ s) e1 . (as [_ . _] sel)])
      (List.mem s assoc_left_parsed_op_list))
      (letrec
       (((loop e1)
          (lambda_match
           ([] e1)
           ([e2 . el] (loop (op_apply loc e1 e2 s) el)))))
       (loop (expr_se e1) (List.map expr_se sel))))
     ((when (Sexpr loc [(Slid _ s) . (as [_ _ . _] sel)])
      (List.mem s assoc_right_parsed_op_list))
      (letrec
       ((loop
          (lambda_match
           ([]
            (assert False))
           ([e1] e1)
           ([e1 . el] (let ((e2 (loop el))) (op_apply loc e1 e2 s))))))
       (loop (List.map expr_se sel))))
     ((when (Sexpr loc [(Slid _ s) . (as [_ _ . _] sel)])
      (List.mem s and_by_couple_op_list))
      (letrec
       ((loop
          (lambda_match
           ((or [] [_]) (assert False))
           ([e1 e2] <:expr< $lid:s$ $e1$ $e2$ >>)
           ([e1 . (as [e2 _ . _] el)]
            (let* ((a1 (op_apply loc e1 e2 s))
                   (a2 (loop el)))
               <:expr< $a1$ && $a2$ >>)))))
       (loop (List.map expr_se sel))))
     ((Sexpr loc [(Stid _ s) se])
      (let ((e (expr_se se))) <:expr< ~ $s$ : $e$ >>))
     ((Sexpr loc [(Slid _ "-") se])
      (let ((e (expr_se se))) <:expr< - $e$ >>))
     ((Sexpr loc [(Slid _ "if") se se1])
      (let* ((e (expr_se se))
             (e1 (expr_se se1)))
         <:expr< if $e$ then $e1$ else () >>))
     ((Sexpr loc [(Slid _ "if") se se1 se2])
      (let* ((e (expr_se se))
             (e1 (expr_se se1))
             (e2 (expr_se se2)))
         <:expr< if $e$ then $e1$ else $e2$ >>))
     ((Sexpr loc [(Slid _ "cond") . sel])
      (letrec
       ((loop
          (lambda_match
           ([(Sexpr loc [(Slid _ "else") . sel])] (begin_se loc sel))
           ([(Sexpr loc [se1 . sel1]) . sel]
            (let* ((e1 (expr_se se1))
                   (e2 (begin_se loc sel1))
                   (e3 (loop sel)))
               <:expr< if $e1$ then $e2$ else $e3$ >>))
           ([] <:expr< () >>)
           ([se . _] (error se "cond clause")))))
       (loop sel)))
     ((Sexpr loc [(Slid _ "while") se . sel])
      (let* ((e (expr_se se))
             (el (List.map expr_se sel)))
         <:expr< while $e$ do { $list:el$ } >>))
     ((Sexpr loc [(Slid _ "for") (Slid _ i) se1 se2 . sel])
      (let* ((i (Pcaml.rename_id.val i))
             (e1 (expr_se se1))
             (e2 (expr_se se2))
             (el (List.map expr_se sel)))
         <:expr< for $i$ = $e1$ to $e2$ do { $list:el$ } >>))
     ((Sexpr loc [(Slid loc1 "lambda")]) <:expr< fun [] >>)
     ((Sexpr loc [(Slid loc1 "lambda") sep . sel])
      (let ((e (begin_se loc1 sel)))
         (match (ipatt_opt_se sep)
          ((Left p) <:expr< fun $p$ -> $e$ >>)
          ((Right (values se sel))
           (List.fold_right
            (lambda (se e)
              (let ((p (ipatt_se se))) <:expr< fun $p$ -> $e$ >>))
            [se . sel] e)))))
     ((Sexpr loc [(Slid _ "lambda_match") . sel])
      (let ((pel (List.map (match_case loc) sel)))
         <:expr< fun [ $list:pel$ ] >>))
     ((Sexpr loc [(Slid _ (as (or "let" "letrec") r)) . sel])
      (match sel
       ([(Sexpr _ sel1) . sel2]
        (let* ((r (= r "letrec"))
               (lbs (List.map let_binding_se sel1))
               (e (begin_se loc sel2)))
           <:expr< let $opt:r$ $list:lbs$ in $e$ >>))
       ([(Slid _ n) (Sexpr _ sl) . sel]
        (let* ((n (Pcaml.rename_id.val n))
               ((values pl el)
                 (List.fold_right
                  (lambda (se (values pl el))
                    (match se
                           ((Sexpr _ [se1 se2])
                            (values [(patt_se se1) . pl]
                                    [(expr_se se2) . el]))
                           (se (error se "named let"))))
                  sl (values [] [])))
               (e1
                 (List.fold_right
                  (lambda (p e) <:expr< fun $p$ -> $e$ >>)
                  pl (begin_se loc sel)))
               (e2
                 (List.fold_left
                  (lambda (e1 e2) <:expr< $e1$ $e2$ >>)
                  <:expr< $lid:n$ >> el)))
           <:expr< let rec $lid:n$ = $e1$ in $e2$ >>))
       ([se . _] (error se "let_binding"))
       (_ (error_loc loc "let_binding"))))
     ((Sexpr loc [(Slid _ "let*") . sel])
      (match sel
       ([(Sexpr _ sel1) . sel2]
        (List.fold_right
         (lambda (se ek)
           (let (((values p e) (let_binding_se se)))
              <:expr< let $p$ = $e$ in $ek$ >>))
         sel1 (begin_se loc sel2)))
       ([se . _] (error se "let_binding"))
       (_ (error_loc loc "let_binding"))))
     ((Sexpr loc [(Slid _ "match") se . sel])
      (let* ((e (expr_se se))
             (pel (List.map (match_case loc) sel)))
         <:expr< match $e$ with [ $list:pel$ ] >>))
     ((Sexpr loc [(Slid _ "parser") . sel])
      (let ((e
              (match sel
               ([(as (Slid _ _) se) . sel]
                (let* ((p (patt_se se))
                       (pc (parser_cases_se loc sel)))
                   <:expr< let $p$ = Stream.count $lid:strm_n$ in $pc$ >>))
               (_ (parser_cases_se loc sel)))))
         <:expr< fun ($lid:strm_n$ : Stream.t _) -> $e$ >>))
     ((Sexpr loc [(Slid _ "match_with_parser") se . sel])
      (let* ((me (expr_se se))
             ((values bpo sel)
               (match sel
                ([(as (Slid _ _) se) . sel] (values (Some (patt_se se)) sel))
                (_ (values None sel))))
             (pc (parser_cases_se loc sel))
             (e
               (match bpo
                ((Some bp)
                 <:expr< let $bp$ = Stream.count $lid:strm_n$ in $pc$ >>)
                (None pc))))
         (match me
          ((when <:expr< $lid:x$ >> (= x strm_n)) e)
          (_ <:expr< let ($lid:strm_n$ : Stream.t _) = $me$ in $e$ >>))))
     ((Sexpr loc [(Slid _ "try") se . sel])
      (let* ((e (expr_se se))
             (pel (List.map (match_case loc) sel)))
         <:expr< try $e$ with [ $list:pel$ ] >>))
     ((Sexpr loc [(Slid _ "begin") . sel])
      (let ((el (List.map expr_se sel))) <:expr< do { $list:el$ } >>))
     ((Sexpr loc [(Slid _ ":=") se1 se2])
      (let* ((e1 (expr_se se1))
             (e2 (expr_se se2)))
         <:expr< $e1$ := $e2$ >>))
     ((Sexpr loc [(Slid _ "values") . sel])
      (let ((el (List.map expr_se sel))) <:expr< ( $list:el$ ) >>))
     ((Srec loc [(Slid _ "with") se . sel])
      (let* ((e (expr_se se))
             (lel (List.map (label_expr_se loc) sel)))
         <:expr< { ($e$) with $list:lel$ } >>))
     ((Srec loc sel)
      (let ((lel (List.map (label_expr_se loc) sel)))
         <:expr< { $list:lel$ } >>))
     ((Sexpr loc [(Slid _ ":") se1 se2])
      (let* ((e (expr_se se1)) (t (ctyp_se se2))) <:expr< ( $e$ : $t$ ) >>))
     ((Sexpr loc [se]) (let ((e (expr_se se))) <:expr< $e$ () >>))
     ((Sexpr loc [(Slid _ "assert") (Suid _ "False")])
        <:expr< assert False >>)
     ((Sexpr loc [(Slid _ "assert") se])
        (let ((e (expr_se se))) <:expr< assert $e$ >>))
     ((Sexpr loc [(Slid _ "lazy") se])
        (let ((e (expr_se se))) <:expr< lazy $e$ >>))
     ((Sexpr loc [se . sel])
      (List.fold_left
       (lambda (e se) (let ((e1 (expr_se se))) <:expr< $e$ $e1$ >>))
       (expr_se se) sel))
     ((Slist loc sel)
      (letrec ((loop
                 (lambda_match
                  ([] <:expr< [] >>)
                  ([se1 (Slid _ ".") se2]
                   (let* ((e (expr_se se1))
                          (el (expr_se se2)))
                     <:expr< [$e$ :: $el$] >>))
                  ([se . sel]
                   (let* ((e (expr_se se))
                          (el (loop sel)))
                     <:expr< [$e$ :: $el$] >>)))))
           (loop sel)))
     ((Squot loc typ txt)
      (Pcaml.handle_expr_quotation loc (values typ txt)))))
  ((begin_se loc)
   (lambda_match
    ([] <:expr< () >>)
    ([se] (expr_se se))
    ((sel)
      (let* ((el (List.map expr_se sel))
             (loc (values (fst (loc_of_sexpr (List.hd sel))) (snd loc))))
         <:expr< do { $list:el$ } >>))))
  (let_binding_se
   (lambda_match
    ((Sexpr loc [se . sel])
       (let ((e (begin_se loc sel)))
         (match (ipatt_opt_se se)
           ((Left p) (values p e))
           ((Right _) (fun_binding_se se e)))))
    (se (error se "let_binding"))))
  ((fun_binding_se se e)
   (match se
          ((Sexpr _ [(Slid _ "values") . _]) (values (ipatt_se se) e))
          ((Sexpr _ [(Slid loc s) . sel])
           (let* ((s (Pcaml.rename_id.val s))
                  (e
                   (List.fold_right
                    (lambda (se e)
                      (let* ((loc
                              (values (fst (loc_of_sexpr se))
                                      (snd (MLast.loc_of_expr e))))
                             (p (ipatt_se se)))
                        <:expr< fun $p$ -> $e$ >>))
                    sel e))
                  (p <:patt< $lid:s$ >>))
             (values p e)))
          ((_) (values (ipatt_se se) e))))
  ((match_case loc)
   (lambda_match
    ((Sexpr loc [(Sexpr _ [(Slid _ "when") se sew]) . sel])
     (values (patt_se se) (Some (expr_se sew)) (begin_se loc sel)))
    ((Sexpr loc [se . sel])
     (values (patt_se se) None (begin_se loc sel)))
    (se (error se "match_case"))))
  ((label_expr_se loc)
   (lambda_match
    ((Sexpr _ [se1 se2]) (values (patt_se se1) (expr_se se2)))
    (se (error se "label_expr"))))
  ((label_patt_se loc)
   (lambda_match
    ((Sexpr _ [se1 se2]) (values (patt_se se1) (patt_se se2)))
    (se (error se "label_patt"))))
  ((parser_cases_se loc)
   (lambda_match
    ([] <:expr< raise Stream.Failure >>)
    ([(Sexpr loc [(Sexpr _ spsel) . act]) . sel]
      (let* ((ekont (lambda _ (parser_cases_se loc sel)))
             (act (match act
                         ([se] (expr_se se))
                         ([sep se]
                               (let* ((p (patt_se sep))
                                      (e (expr_se se)))
                        <:expr< let $p$ = Stream.count $lid:strm_n$ in $e$ >>))
                         (_ (error_loc loc "parser_case")))))
        (stream_pattern_se loc act ekont spsel)))
    ([se . _]
         (error se "parser_case"))))
  ((stream_pattern_se loc act ekont)
   (lambda_match
    ([] act)
    ([se . sel]
         (let* ((ckont (lambda err <:expr< raise (Stream.Error $err$) >>))
                (skont (stream_pattern_se loc act ckont sel)))
           (stream_pattern_component skont ekont <:expr< "" >> se)))))
  ((stream_pattern_component skont ekont err)
   (lambda_match
    ((Sexpr loc [(Slid _ "`") se . wol])
     (let* ((wo (match wol
                       ([se] (Some (expr_se se)))
                       ([] None)
                       (_ (error_loc loc "stream_pattern_component"))))
            (e (peek_fun loc))
            (p (patt_se se))
            (j (junk_fun loc))
            (k (ekont err)))
       <:expr< match $e$ $lid:strm_n$ with
               [ Some $p$ $when:wo$ -> do { $j$ $lid:strm_n$ ; $skont$ }
               | _ -> $k$ ] >>))
    ((Sexpr loc [se1 se2])
     (let* ((p (patt_se se1))
            (e (let ((e (expr_se se2)))
       <:expr< try Some ($e$ $lid:strm_n$) with [ Stream.Failure -> None ] >>))
            (k (ekont err)))
       <:expr< match $e$ with [ Some $p$ -> $skont$ | _ -> $k$ ] >>))
    ((Sexpr loc [(Slid _ "?") se1 se2])
     (stream_pattern_component skont ekont (expr_se se2) se1))
    ((Slid loc s)
     (let ((s (Pcaml.rename_id.val s)))
        <:expr< let $lid:s$ = $lid:strm_n$ in $skont$ >>))
    (se
     (error se "stream_pattern_component"))))
  (patt_se
   (lambda_match
    ((Sacc loc se1 se2)
     (let* ((p1 (patt_se se1)) (p2 (patt_se se2))) <:patt< $p1$ . $p2$ >>))
    ((Slid loc "_") <:patt< _ >>)
    ((Slid loc s) <:patt< $lid:(Pcaml.rename_id.val s)$ >>)
    ((Suid loc s) <:patt< $uid:(Pcaml.rename_id.val s)$ >>)
    ((Sint loc s) <:patt< $int:s$ >>)
    ((Sfloat loc s) <:patt< $flo:s$ >>)
    ((Schar loc s) <:patt< $chr:s$ >>)
    ((Sstring loc s) <:patt< $str:s$ >>)
    ((Stid loc _) (error_loc loc "patt"))
    ((Sqid loc _) (error_loc loc "patt"))
    ((Srec loc sel)
     (let ((lpl (List.map (label_patt_se loc) sel)))
        <:patt< { $list:lpl$ } >>))
    ((Sexpr loc [(Slid _ ":") se1 se2])
     (let* ((p (patt_se se1)) (t (ctyp_se se2))) <:patt< ($p$ : $t$) >>))
    ((Sexpr loc [(Slid _ "or") se . sel])
     (List.fold_left
      (lambda (p se) (let ((p1 (patt_se se))) <:patt< $p$ | $p1$ >>))
      (patt_se se) sel))
    ((Sexpr loc [(Slid _ "range") se1 se2])
     (let* ((p1 (patt_se se1)) (p2 (patt_se se2))) <:patt< $p1$ .. $p2$ >>))
    ((Sexpr loc [(Slid _ "values") . sel])
     (let ((pl (List.map patt_se sel))) <:patt< ( $list:pl$ ) >>))
    ((Sexpr loc [(Slid _ "as") se1 se2])
     (let* ((p1 (patt_se se1))
            (p2 (patt_se se2)))
       <:patt< ($p1$ as $p2$) >>))
    ((Sexpr loc [se . sel])
     (List.fold_left
      (lambda (p se) (let ((p1 (patt_se se))) <:patt< $p$ $p1$ >>))
      (patt_se se) sel))
    ((Sexpr loc []) <:patt< () >>)
    ((Slist loc sel)
     (letrec ((loop
                (lambda_match
                 ([] <:patt< [] >>)
                 ([se1 (Slid _ ".") se2]
                  (let* ((p (patt_se se1))
                         (pl (patt_se se2)))
                    <:patt< [$p$ :: $pl$] >>))
                 ([se . sel]
                  (let* ((p (patt_se se))
                         (pl (loop sel)))
                    <:patt< [$p$ :: $pl$] >>)))))
          (loop sel)))
    ((Squot loc typ txt)
     (Pcaml.handle_patt_quotation loc (values typ txt)))))
  ((ipatt_se se)
   (match (ipatt_opt_se se)
          ((Left p) p)
          ((Right (values se _)) (error se "ipatt"))))
  (ipatt_opt_se
   (lambda_match
    ((Slid loc "_") (Left <:patt< _ >>))
    ((Slid loc s) (Left <:patt< $lid:(Pcaml.rename_id.val s)$ >>))
    ((Stid loc s) (Left <:patt< ~ $(Pcaml.rename_id.val s)$ >>))
    ((Sqid loc s) (Left <:patt< ? $(Pcaml.rename_id.val s)$ >>))
    ((Sexpr loc [(Sqid _ s) se])
     (let* ((s (Pcaml.rename_id.val s))
            (e (expr_se se)))
        (Left <:patt< ? ( $lid:s$ = $e$ ) >>)))
    ((Sexpr loc [(Slid _ ":") se1 se2])
     (let* ((p (ipatt_se se1)) (t (ctyp_se se2)))
        (Left <:patt< ($p$ : $t$) >>)))
    ((Sexpr loc [(Slid _ "values") . sel])
     (let ((pl (List.map ipatt_se sel))) (Left <:patt< ( $list:pl$ ) >>)))
    ((Sexpr loc []) (Left <:patt< () >>))
    ((Sexpr loc [se . sel]) (Right (values se sel)))
    (se (error se "ipatt"))))
  (type_declaration_list_se
   (lambda_match
    ([se1 se2 . sel]
     (let (((values n1 loc1 tpl)
            (match se1
                   ((Sexpr _ [(Slid loc n) . sel])
                    (values n loc (List.map type_parameter_se sel)))
                   ((Slid loc n)
                    (values n loc []))
                   ((se)
                    (error se "type declaration")))))
       [(values (values loc1 (Pcaml.rename_id.val n1)) tpl (ctyp_se se2) []) .
        (type_declaration_list_se sel)]))
    ([] [])
    ([se . _] (error se "type_declaration"))))
  (type_parameter_se
   (lambda_match
    ((when (Slid _ s) (and (>= (String.length s) 2) (= s.[0] ''')))
     (values (String.sub s 1 (- (String.length s) 1)) (values False False)))
    (se
     (error se "type_parameter"))))
  (ctyp_se
   (lambda_match
    ((Sexpr loc [(Slid _ "sum") . sel])
     (let ((cdl (List.map constructor_declaration_se sel)))
       <:ctyp< [ $list:cdl$ ] >>))
    ((Srec loc sel)
     (let ((ldl (List.map label_declaration_se sel)))
       <:ctyp< { $list:ldl$ } >>))
    ((Sexpr loc [(Slid _ "->") . (as [_ _ . _] sel)])
     (letrec
        ((loop
            (lambda_match
             ([] (assert False))
             ([se] (ctyp_se se))
             ([se . sel]
               (let* ((t1 (ctyp_se se))
                      (loc (values (fst (loc_of_sexpr se)) (snd loc)))
                      (t2 (loop sel)))
                   <:ctyp< $t1$ -> $t2$ >>)))))
        (loop sel)))
    ((Sexpr loc [(Slid _ "*") . sel])
     (let ((tl (List.map ctyp_se sel))) <:ctyp< ($list:tl$) >>))
    ((Sexpr loc [se . sel])
     (List.fold_left
      (lambda (t se) (let ((t2 (ctyp_se se))) <:ctyp< $t$ $t2$ >>))
      (ctyp_se se) sel))
    ((Sacc loc se1 se2)
     (let* ((t1 (ctyp_se se1)) (t2 (ctyp_se se2))) <:ctyp< $t1$ . $t2$ >>))
    ((Slid loc "_") <:ctyp< _ >>)
    ((Slid loc s)
     (if (= s.[0] ''')
         (let ((s (String.sub s 1 (- (String.length s) 1))))
           <:ctyp< '$s$ >>)
         <:ctyp< $lid:(Pcaml.rename_id.val s)$ >>))
    ((Suid loc s) <:ctyp< $uid:(Pcaml.rename_id.val s)$ >>)
    (se (error se "ctyp"))))
  (constructor_declaration_se
   (lambda_match
    ((Sexpr loc [(Suid _ ci) . sel])
     (values loc (Pcaml.rename_id.val ci) (List.map ctyp_se sel)))
    (se
     (error se "constructor_declaration"))))
  (label_declaration_se
   (lambda_match
    ((Sexpr loc [(Slid _ lab) (Slid _ "mutable") se])
     (values loc (Pcaml.rename_id.val lab) True (ctyp_se se)))
    ((Sexpr loc [(Slid _ lab) se])
     (values loc (Pcaml.rename_id.val lab) False (ctyp_se se)))
    (se
     (error se "label_declaration")))))

(define directive_se
  (lambda_match
   ((Sexpr _ [(Slid _ s)]) (values s None))
   ((Sexpr _ [(Slid _ s) se]) (let ((e (expr_se se))) (values s (Some e))))
   (se (error se "directive"))))

; Parser

(:= Pcaml.syntax_name.val "Scheme")
(:= Pcaml.no_constructors_arity.val False)

(begin
 (Grammar.Unsafe.gram_reinit gram (lexer_gmake ()))
 (Grammar.Unsafe.clear_entry interf)
 (Grammar.Unsafe.clear_entry implem)
 (Grammar.Unsafe.clear_entry top_phrase)
 (Grammar.Unsafe.clear_entry use_file)
 (Grammar.Unsafe.clear_entry module_type)
 (Grammar.Unsafe.clear_entry module_expr)
 (Grammar.Unsafe.clear_entry sig_item)
 (Grammar.Unsafe.clear_entry str_item)
 (Grammar.Unsafe.clear_entry expr)
 (Grammar.Unsafe.clear_entry patt)
 (Grammar.Unsafe.clear_entry ctyp)
 (Grammar.Unsafe.clear_entry let_binding)
 (Grammar.Unsafe.clear_entry type_declaration)
 (Grammar.Unsafe.clear_entry class_type)
 (Grammar.Unsafe.clear_entry class_expr)
 (Grammar.Unsafe.clear_entry class_sig_item)
 (Grammar.Unsafe.clear_entry class_str_item))

(:= Pcaml.parse_interf.val (Grammar.Entry.parse interf))
(:= Pcaml.parse_implem.val (Grammar.Entry.parse implem))

(define sexpr (Grammar.Entry.create gram "sexpr"))

(definerec leftify
  (lambda_match
    ((Sacc loc1 se1 se2)
     (match (leftify se2)
       ((Sacc loc2 se2 se3) (Sacc loc1 (Sacc loc2 se1 se2) se3))
       (se2 (Sacc loc1 se1 se2))))
    (x x)))

EXTEND
  GLOBAL : implem interf top_phrase use_file str_item sig_item expr
    patt sexpr /
  implem :
    [ [ "#" / se = sexpr ->
          (let (((values n dp) (directive_se se)))
             (values [(values <:str_item< # $n$ $opt:dp$ >> loc)] True))
      | si = str_item / x = SELF ->
          (let* (((values sil stopped) x)
                 (loc (MLast.loc_of_str_item si)))
             (values [(values si loc) . sil] stopped))
      | EOI -> (values [] False) ] ]
  /
  interf :
    [ [ "#" / se = sexpr ->
          (let (((values n dp) (directive_se se)))
             (values [(values <:sig_item< # $n$ $opt:dp$ >> loc)] True))
      | si = sig_item / x = SELF ->
          (let* (((values sil stopped) x)
                 (loc (MLast.loc_of_sig_item si)))
             (values [(values si loc) . sil] stopped))
      | EOI -> (values [] False) ] ]
  /
  top_phrase :
    [ [ "#" / se = sexpr ->
          (let (((values n dp) (directive_se se)))
             (Some <:str_item< # $n$ $opt:dp$ >>))
      | se = sexpr -> (Some (str_item_se se))
      | EOI -> None ] ]
  /
  use_file :
    [ [ "#" / se = sexpr ->
          (let (((values n dp) (directive_se se)))
             (values [<:str_item< # $n$ $opt:dp$ >>] True))
      | si = str_item / x = SELF ->
          (let (((values sil stopped) x)) (values [si . sil] stopped))
      | EOI -> (values [] False) ] ]
  /
  str_item :
    [ [ se = sexpr -> (str_item_se se)
      | e = expr -> <:str_item< $exp:e$ >> ] ]
  /
  sig_item :
    [ [ se = sexpr -> (sig_item_se se) ] ]
  /
  expr :
    [ "top"
      [ se = sexpr -> (expr_se se) ] ]
  /
  patt :
    [ [ se = sexpr -> (patt_se se) ] ]
  /
  sexpr :
    [ [ se1 = sexpr_dot / se2 = sexpr -> (leftify (Sacc loc se1 se2)) ]
    | [ "(" / sl = LIST0 sexpr / ")" -> (Sexpr loc sl)
      | "(" / sl = LIST0 sexpr / ")." / se = sexpr ->
          (leftify (Sacc loc (Sexpr loc sl) se))
      | "[" / sl = LIST0 sexpr / "]" -> (Slist loc sl)
      | "{" / sl = LIST0 sexpr / "}" -> (Srec loc sl)
      | a = pa_extend_keyword -> (Slid loc a)
      | s = LIDENT -> (Slid loc s)
      | s = UIDENT -> (Suid loc s)
      | s = TILDEIDENT -> (Stid loc s)
      | s = QUESTIONIDENT -> (Sqid loc s)
      | s = INT -> (Sint loc s)
      | s = FLOAT -> (Sfloat loc s)
      | s = CHAR -> (Schar loc s)
      | s = STRING -> (Sstring loc s)
      | s = QUOT ->
          (let* ((i (String.index s ':'))
                 (typ (String.sub s 0 i))
                 (txt (String.sub s (+ i 1) (- (- (String.length s) i) 1))))
            (Squot loc typ txt)) ] ]
  /
  sexpr_dot :
    [ [ s = LIDENTDOT -> (Slid loc s)
      | s = UIDENTDOT -> (Suid loc s) ] ]
  /
  pa_extend_keyword :
    [ [ "_" -> "_"
      | "," -> ","
      | "=" -> "="
      | ":" -> ":"
      | "." -> "."
      | "/" -> "/" ] ]
  /
END
