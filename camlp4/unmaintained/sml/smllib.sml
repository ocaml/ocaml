(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

datatype 'a option = SOME of 'a | NONE
exception Fail of string
exception Domain
exception Subscript
type 'a vector = 'a array

structure OCaml =
  struct
    structure List = List
    structure String = String
  end

structure Time =
  struct
    datatype time = TIME of { sec : int, usec : int }
    fun toString _ = failwith "not implemented Time.toString"
    fun now _ = failwith "not implemented Time.now"
  end

datatype cpu_timer =
  CPUT of { gc : Time.time, sys : Time.time, usr : Time.time }

datatype real_timer =
  RealT of Time.time

structure Char =
  struct
    val ord = Char.code
  end

structure General =
  struct
    datatype order = LESS | EQUAL | GREATER
  end
type order = General.order == LESS | EQUAL | GREATER

structure OS =
  struct
    exception SysErr
    structure Path =
      struct
        fun dir s =
          let val r = Filename.dirname s in
            if r = "." then "" else r
          end
        val file = Filename.basename
        fun ext s =
          let fun loop i =
            if i < 0 then NONE
            else if String.get s i = #"." then
              let val len = String.length s - i - 1 in
                if len = 0 then NONE else SOME (String.sub s (i + 1) len)
              end
            else loop (i - 1)
          in
            loop (String.length s - 1)
          end
        fun splitDirFile s =
          {dir = Filename.dirname s,
           file = Filename.basename s}
        fun joinDirFile x =
          let val {dir,file} = x in Filename.concat dir file end
      end
    structure FileSys =
      struct
        datatype access_mode = A_READ | A_WRITE | A_EXEC
        val chDir = Sys.chdir
        fun isDir s =
          (Unix.stat s) ocaml_record_access Unix.st_kind = Unix.S_DIR
          handle Unix.Unix_error _ => raise SysErr
        fun access (s, accs) =
          let val st = Unix.stat s
              val prm = st ocaml_record_access Unix.st_perm
              val prm =
                if st ocaml_record_access Unix.st_uid = Unix.getuid () then
                  lsr prm 6
                else if st ocaml_record_access Unix.st_uid = Unix.getgid ()
                then
                  lsr prm 3
                else prm
              val rf =
                if List.mem A_READ accs then land prm 4 <> 0 else true
              val wf =
                if List.mem A_WRITE accs then land prm 2 <> 0 else true
              val xf =
                if List.mem A_EXEC accs then land prm 1 <> 0 else true
          in
            rf andalso wf andalso xf
          end
          handle Unix.Unix_error (_, f, _) =>
            if f = "stat" then false else raise SysErr
      end
    structure Process =
      struct
        fun system s = (flush stdout; flush stderr; Sys.command s)
        fun getEnv s = SOME (Sys.getenv s) handle Not_found => NONE
        val success = 0
      end
  end

exception SysErr = OS.SysErr

structure IO =
  struct
    exception Io of {cause:exn, function:string, name:string}
  end

structure TextIO =
  struct
    type instream = in_channel * char option option ref
    type outstream = out_channel
    type elem = char
    type vector = string
    fun openIn fname =
      (open_in fname, ref NONE) handle exn =>
        raise IO.Io {cause = exn, function = "openIn", name = fname}
    val openOut = open_out
    fun closeIn (ic, ahc) = (ahc := SOME NONE; close_in ic)
    val closeOut = close_out
    val stdIn = (stdin, ref (NONE : char option option))
    fun endOfStream (ic, _) = pos_in ic = in_channel_length ic
    fun inputLine (ic, ahc) =
      case !ahc of
        NONE =>
          (input_line ic ^ "\n" handle End_of_file => (ahc := SOME NONE; ""))
      | SOME NONE => ""
      | SOME (SOME c) =>
          (ahc := NONE;
           if c = #"\n" then "\n"
           else
             String.make 1 c ^ input_line ic ^ "\n" handle
               End_of_file => (ahc := SOME NONE; ""))
    fun input1 (ic, ahc) =
      case !ahc of
        NONE =>
          (SOME (input_char ic) handle End_of_file => (ahc := SOME NONE; NONE))
      | SOME NONE => NONE
      | SOME x => (ahc := NONE; x)
    fun inputN (ins, n) =
      let fun loop n =
        if n <= 0 then ""
        else
          case input1 ins of
            SOME c => String.make 1 c ^ loop (n - 1)
          | NONE => ""
      in
        loop n
      end
    fun output (oc, v) = output_string oc v
    fun inputAll ic = failwith "not implemented TextIO.inputAll"
    fun lookahead (ic, ahc) =
      case !ahc of
        NONE => let val r = SOME (input_char ic) in ahc := SOME r; r end
      | SOME x => x
    fun print s = (print_string s; flush stdout)
  end

structure Timer =
  struct
    fun startRealTimer () = failwith "not implemented Timer.startRealTimer"
    fun startCPUTimer () = failwith "not implemented Timer.startCPUTimer"
    fun checkRealTimer _ = failwith "not implemented Timer.checkRealTimer"
    fun checkCPUTimer _ = failwith "not implemented Timer.checkCPUTimer"
  end

structure Date =
  struct
    datatype month =
      Jan | Feb | Mar | Apr | May | Jun | Jul | Sep | Oct | Nov | Dec
    datatype wday = Sun | Mon | Tue | Wed | Thu | Fri | Sat
    datatype date =
      DATE of
        {day : int, hour : int, isDst : bool option, minute : int,
         month : month, offset : int option, second : int, wday : wday,
         yday : int, year : int}
    fun fmt _ _ = failwith "not implemented Date.fmt"
    fun fromTimeLocal _ = failwith "not implemented Date.fromTimeLocal"
  end

structure Posix =
  struct
    structure ProcEnv =
      struct
        fun getenv s = SOME (Sys.getenv s) handle Not_found => NONE
      end
  end

structure SMLofNJ =
  struct
    fun exportML s = failwith ("not implemented exportML " ^ s)
  end

fun null x = x = []
fun explode s =
  let fun loop i =
    if i = String.length s then []
    else String.get s i :: loop (i + 1)
  in
    loop 0
  end

val app = List.iter
fun implode [] = ""
  | implode (c :: l) = String.make 1 c ^ implode l

fun ooo f g x = f (g x)

structure Array =
  struct
    fun array (len, v) = Array.create len v
    fun sub _ = failwith "not implemented Array.sub"
    fun update _ = failwith "not implemented Array.update"
    (* for make the profiler work *)
    val set = Array.set
    val get = Array.get
  end

structure Vector =
  struct
    fun tabulate _ = failwith "not implemented Vector.tabulate"
    fun sub _ = failwith "not implemented Vector.sub"
  end

structure Bool =
  struct
    val toString = string_of_bool
  end

structure String =
  struct
    val size = String.length
    fun substring (s, beg, len) =
      String.sub s beg len handle Invalid_argument _ => raise Subscript
    val concat = String.concat ""
    fun sub (s, i) = String.get s i
    val str = String.make 1
    fun compare (s1, s2) =
      if s1 < s2 then LESS
      else if s1 > s2 then GREATER
      else EQUAL
    fun isPrefix s1 s2 =
      let fun loop i1 i2 =
        if i1 >= String.length s1 then true
        else if i2 >= String.length s2 then false
        else if String.get s1 i1 = String.get s2 i2 then loop (i1 + 1) (i2 + 1)
        else false
      in
        loop 0 0
      end
    fun tokens p s =
      let fun loop tok i =
        if i >= String.length s then
          if tok = "" then [] else [tok]
        else if p (String.get s i) then
          if tok <> "" then tok :: loop "" (i + 1)
          else loop "" (i + 1)
        else loop (tok ^ String.make 1 (String.get s i)) (i + 1)
      in
        loop "" 0
      end
    fun extract _ = failwith "not implemented String.extract"
  end

structure Substring =
  struct
    type substring = string * int * int
    fun string (s : substring) = String.substring s
    fun all s : substring = (s, 0, String.size s)
    fun splitl f ((s, beg, len) : substring) : substring * substring =
      let fun loop di =
        if di = len then ((s, beg, len), (s, 0, 0))
        else if f (String.sub (s, beg + di)) then loop (di + 1)
        else ((s, beg, di), (s, beg + di, len - di))
      in
        loop 0
      end
    fun getc (s, i, len) =
      if len > 0 andalso i < String.size s then
        SOME (String.sub (s, i), (s, i+1, len-1))
      else NONE
    fun slice _ = failwith "not implemented: Substring.slice"
    fun isEmpty (s, beg, len) = len = 0
    fun concat sl = String.concat (List.map string sl)
  end
type substring = Substring.substring

structure StringCvt =
  struct
    datatype radix = BIN | OCT | DEC | HEX
    type ('a, 'b) reader = 'b -> ('a * 'b) option 
  end

structure ListPair =
  struct
    fun zip (a1::l1, a2::l2) = (a1, a2) :: zip (l1, l2)
      | zip _ = []
    val unzip = List.split
    fun all f (x1 :: l1, x2 :: l2) = f (x1, x2) andalso all f (l1, l2)
      | all _ _ = true
    fun map f (a1::l1, a2::l2) =
          let val r = f (a1, a2) in r :: map f (l1, l2) end
      | map _ _ = []
  end

structure ListMergeSort =
  struct
    fun uniqueSort cmp l =
      List.sort
       (fn x => fn y =>
          case cmp (x, y) of
            LESS => ~1
          | EQUAL => 0
          | GREATER => 1)
       l
  end

structure List =
  struct
    exception Empty
    fun hd [] = raise Empty
      | hd (x :: l) = x
    fun tl [] = raise Empty
      | tl (x :: l) = l
    fun foldr f a l =
      let fun loop a [] = a
            | loop a (x :: l) = loop (f (x, a)) l
      in
        loop a (List.rev l)
      end
    fun foldl f a l = List.fold_left (fn a => fn x => f (x, a)) a l
    val concat = List.flatten
    val exists = List.exists
    val filter = List.filter
    val length = List.length
    val map = List.map
    val rev = List.rev
    val all = List.for_all
    fun find f [] = NONE
      | find f (x :: l) = if f x then SOME x else find f l
    fun last s =
      case List.rev s of
        [] => raise Empty
      | x :: _ => x
    fun take _ = failwith "not implemented: List.take"
    fun partition _ = failwith "not implemented: List.partition"
    fun mapPartial f [] = []
      | mapPartial f (x :: l) =
          case f x of
            NONE => mapPartial f l
          | SOME y => y :: mapPartial f l
    fun op @ l1 l2 = List.rev_append (List.rev l1) l2
  end

structure Int =
  struct
    type int1 = int
    type int = int1
    val toString = string_of_int
    fun fromString s = SOME (int_of_string s) handle Failure _ => NONE
    fun min (x, y) = if x < y then x else y
    fun max (x, y) = if x > y then x else y
    fun scan radix getc src = failwith "not impl: Int.scan"
  end

val foldr = List.foldr
val exists = List.exists
val size = String.size
val substring = String.substring
val concat = String.concat
val length = List.length
val op @ = List.op @
val hd = List.hd
val tl = List.tl
val map = List.map
val rev = List.rev
val use_hook = ref (fn (s : string) => (failwith "no defined directive use" : unit))
fun use s = !use_hook s
fun isSome (SOME _) = true
  | isSome NONE = false
fun valOf (SOME x) = x
  | valOf NONE = failwith "valOf"
val print = TextIO.print
