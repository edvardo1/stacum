fun say (strs: string list): unit =
    print (String.concat (strs @ ["\n"]))

fun slurp_file (filename: string): string  =
    let val in_stream = TextIO.openIn filename
        val content = TextIO.inputAll in_stream
        val _ = TextIO.closeIn in_stream
    in content end

datatype 'a sequence = VECTOR of 'a Vector.vector
                     | SLICE of 'a VectorSlice.slice
fun sequence_to_vector (VECTOR v) = v
  | sequence_to_vector (SLICE  s) = VectorSlice.vector s
fun sequence_foldl (f: ('a * 'b -> 'b)) (init: 'b) (seq: 'a sequence) : 'b =
    Vector.foldl f init (sequence_to_vector seq)

fun sequence_sub (s: 'a sequence, index: int): 'a  =
    case s of
        VECTOR (v) => Vector.sub (v, index)
     |  SLICE (v) => VectorSlice.sub (v, index)
fun sequence_subseq (s: 'a sequence,
                     start: int, length: int option): 'a sequence =
    case s of
        VECTOR (v) => SLICE (VectorSlice.slice (v, start, length))
     |  SLICE (v) =>  SLICE (VectorSlice.subslice (v, start, length))
fun sequence_length (s: 'a sequence): int  =
    case s of
        VECTOR (v) => Vector.length v
     |  SLICE (v) => VectorSlice.length v

fun sequence_of_list (list: 'a list): 'a sequence =
    VECTOR (Vector.fromList list)
fun sequence_of_string str =
    VECTOR (Vector.fromList (String.explode str))

fun opposite_of (a: int, reference: int, length: int): int =
    (length + reference) - a
fun sequence_app (seq: 'a sequence, f: ('a -> unit)): unit =
    let fun helper index =
            if (index < sequence_length seq)
            then (f (sequence_sub(seq, index));
                  helper (index + 1))
            else ()
    in helper 0 end
fun sequence_append (seq: 'a sequence, element: 'a): 'a sequence =
    case seq
     of VECTOR (v) => VECTOR (Vector.concat [v, Vector.fromList [element]])
      | SLICE  (v) =>
        VECTOR (VectorSlice.concat
                    [v, VectorSlice.full
                            (Vector.fromList [element])])
fun sequence_concat (s1: 'a sequence, s2: 'a sequence): 'a sequence =
    VECTOR (Vector.concat (map sequence_to_vector [s1, s2]))

fun sequence_concat_list (seq: 'a sequence list): 'a sequence =
    VECTOR (Vector.concat (map sequence_to_vector seq))

fun sequence_map (seq: 'a sequence, f: ('a -> 'b)): ('b sequence) =
    let fun helper (index: int, acc: 'b sequence): ('b sequence) =
            if (index < sequence_length seq)
            then helper (index + 1,
                         sequence_append (acc, f (sequence_sub (seq, index))))
            else acc
    in helper (0, sequence_of_list []) end

fun sequence_inject (seq: 'a sequence,
                     f: ('a sequence -> 'b sequence)): 'b sequence =
    f seq

fun sequence_reverse (seq: 'a sequence): 'a sequence =
    case seq 
     of VECTOR (v) =>
        VECTOR
            (Vector.mapi
                 (fn (index, elem) =>
                     (sequence_sub (seq,
                                    opposite_of
                                        (index, 0, sequence_length seq - 1))))
                 v)
      | SLICE (v) =>
        VECTOR
            (VectorSlice.mapi
                 (fn (index, elem) =>
                     (sequence_sub (seq,
                                    opposite_of
                                        (index, 0, sequence_length seq - 1))))
                 v)
fun sequence_to_list (seq: 'a sequence): 'a list =
    let fun loop index ls =
            if (index < sequence_length seq)
            then loop (index + 1) (sequence_sub (seq, index) :: ls)
            else List.rev ls
    in loop 0 [] end
fun sequence_take (seq, a) =
    case seq
     of VECTOR (v) => SLICE (VectorSlice.slice (v, 0, SOME a))
      | SLICE (s) => SLICE (VectorSlice.subslice (s, 0, SOME a))
fun sequence_to_string (seq: char sequence): string =
    String.implode (sequence_to_list seq)
fun sequence_vec (VECTOR (v)) = VECTOR (v)
  | sequence_vec (SLICE (v)) = VECTOR (VectorSlice.vector v)

exception IllegalDigit
fun digit_to_number #"0" = 0
  | digit_to_number #"1" = 1
  | digit_to_number #"2" = 2
  | digit_to_number #"3" = 3
  | digit_to_number #"4" = 4
  | digit_to_number #"5" = 5
  | digit_to_number #"6" = 6
  | digit_to_number #"7" = 7
  | digit_to_number #"8" = 8
  | digit_to_number #"9" = 9
  | digit_to_number _ = raise IllegalDigit

fun int_pow (base: int, exp: int): int =
    if exp = 0 then 1
    else if (exp mod 2) = 0 then int_pow (base * base, exp div 2)
    else base * int_pow (base * base, (exp - 1) div 2)

fun sequence_to_integer (seq: char sequence): int =
    let fun helper (min: int, base: int, acc: int, index: int, ret: int) =
            case index < min
             of true => ret
              | false =>
                helper (min,
                        base,
                        acc * base, index - 1,
                        ret +
                        acc *
                        (digit_to_number (sequence_sub (seq, index))))
    in if sequence_sub (seq, 0) = #"-"
       then ~(helper (1, 10, 1, sequence_length seq - 1, 0))
       else helper (0, 10, 1, sequence_length seq - 1, 0)
    end

fun sequence_to_large_integer (seq: char sequence): LargeInt.int =
    let fun helper (min: int, base: LargeInt.int, acc: LargeInt.int,
                    index: int, ret: LargeInt.int) =
            case index < min
             of true => ret
              | false =>
                helper (min,
                        base,
                        acc * base, index - 1,
                        ret +
                        acc *
                        Int.toLarge (digit_to_number (sequence_sub (seq, index))))
    in if sequence_sub (seq, 0) = #"-"
       then ~(helper (1, 10, 1, sequence_length seq - 1, 0))
       else helper (0, 10, 1, sequence_length seq - 1, 0)
    end

fun sequence_eq (s1: char sequence, s2: char sequence): bool =
    if sequence_length s1 = sequence_length s2
    then let fun helper index =
                 if index >= sequence_length s1 then true
                 else
                     (if (sequence_sub (s1, index)) = (sequence_sub (s2, index))
                      then helper (index + 1)
                      else false)
         in helper 0
         end
    else false

fun sequence_member
        (element: ''a, seq: ''a sequence, cmp: ((''a * ''a) -> bool)): bool =
    let fun helper index =
            case index >= sequence_length seq
             of true => false
              | false => case cmp (sequence_sub (seq, index), element)
                          of true => true
                           | _ => helper (index + 1)
    in helper 0 end

type ('a, 'b) table =
     {cmp: (('a * 'a) -> bool), length: int, seq: ('a * 'b) sequence}
fun table_make (cmp: (('a * 'a) -> bool),
                list: ('a * 'b) list)
    : (('a, 'b) table) =
    {cmp=cmp,
     length=List.length list,
     seq=sequence_of_list list}

fun table_add ({cmp=cmp,length=length,seq=seq}: (('a, 'b) table),
               key: 'a, value: 'b)
    : ('a, 'b) table =
    {cmp=cmp,length=length+1,seq=sequence_append (seq, (key, value))}

fun table_access ({cmp=cmp,length=length,seq=seq}: ('a, 'b) table,
                  key: 'a)
    : 'b option =
    let fun helper index =
            case index < length
             of false => NONE
              | true =>
                let val (a, b) = sequence_sub (seq, index)
                in case cmp (key, a)
                    of true => SOME b
                     | false => helper (index + 1)
                end
    in helper 0 end

fun table_concat (t1: ('a, 'b) table, t2: ('a, 'b) table): ('a, 'b) table =
    {cmp=(#cmp t1),
     length=(#length t1) + (#length t2),
     seq=sequence_concat (#seq t1, #seq t2)}
        
exception AssertionFailed of string option
fun assert (condition: bool, message: string option): unit =
    case condition
     of true => ()
      | false => raise AssertionFailed message

datatype IntMode = U64 | U32 | U16 | U8 |
                   I64 | I32 | I16 | I8
datatype TokenType = Identifier
                   | IntegerLiteral
                   | StringLiteral
                   | IntMode of IntMode
                   | LabelDef
                   | MacroDef
                   | MacroEnd
                   | SegmentMarker
       | EndOfFile
type seq_spec = {start: int, length: int}
fun sum_seqspec ({start=a_start, length=_}: seq_spec,
                 {start=b_start, length=b_length}: seq_spec): seq_spec =
    {start=a_start, length=(b_start+b_length) - a_start}
type Token = {token_type: TokenType, indices: seq_spec}
fun make_token (token_type: TokenType, indices: seq_spec): Token =
    {token_type=token_type, indices=indices}

type Lexer = {
    str: char sequence,
    index: int,
    line: int
}

fun lexer_get_index (lexer: Lexer): int = #index lexer
fun lexer_get_seq (lexer: Lexer): char sequence = #str lexer
fun lexer_get_subseq (lexer: Lexer, start: int, length: int): char sequence =
    sequence_subseq ((lexer_get_seq lexer), start, (SOME (length)))
fun lexer_get_seq_max (lexer: Lexer): int =
    sequence_length (lexer_get_seq lexer)
fun lexer_get_line (lexer: Lexer): int = #line lexer

exception ParserTriedToAccessOutOfBoundsToken
fun lexer_get_token_substr (lexer: Lexer, {token_type=token_type,
                                           indices={start=start,
                                                    length=length}}: Token)
    : char sequence =
    if length >= lexer_get_seq_max lexer
    then raise ParserTriedToAccessOutOfBoundsToken
    else lexer_get_subseq (lexer,
                           start,
                           length)

val segment_marker_str = sequence_of_string("segment")
val data_str = sequence_of_string("data")
val code_str = sequence_of_string("code")
val stack_str = sequence_of_string("stack")
val macrodef_str = sequence_of_string("%macrodef")
val macroend_str = sequence_of_string("%macroend")
val int_mode_u8_str = sequence_of_string("&u8")
val int_mode_u16_str = sequence_of_string("&u16")
val int_mode_u32_str = sequence_of_string("&u32")
val int_mode_u64_str = sequence_of_string("&u64")
val int_mode_i8_str = sequence_of_string("&i8")
val int_mode_i16_str = sequence_of_string("&i16")
val int_mode_i32_str = sequence_of_string("&i32")
val int_mode_i64_str = sequence_of_string("&i64")

fun lexer_of_string (strarg: string): Lexer =
    {index=0, str=VECTOR(Vector.fromList(String.explode strarg)), line=1}
val _ = assert (let val mylex = lexer_of_string "this is a string"
                in lexer_get_index mylex = 0 andalso
                   sequence_eq (lexer_get_seq mylex,
                                sequence_of_string"this is a string") andalso
                   lexer_get_line mylex = 1
                end, SOME "lexer_of_string doesn't work!")

fun lexer_current_char (lexer: Lexer): char option =
    let val index = lexer_get_index lexer
        val seq = lexer_get_seq lexer
        val max = sequence_length seq
    in case lexer_get_index lexer >= lexer_get_seq_max lexer
        of true => NONE
         | false => SOME (sequence_sub (lexer_get_seq lexer, index))
    end

fun lexer_next_char
        ({index=index, str=str, line=line}: Lexer): (Lexer * char) option =
    case index < (sequence_length str)
     of true => SOME let val character = sequence_sub (str, index)
                     in ({str=str, index=index+1,
                          line=line + (case character of #"\n" => 1 | _ => 0)},
                         character)
                     end
      | false => NONE

fun lexer_next_charseq_is
        (lexer: Lexer,
         this: char sequence)
    : (Lexer * seq_spec) option =
    let val {index=start, str=str, line=line} = lexer
        fun helper (lexer: Lexer) (index: int): (Lexer * seq_spec) option =
            case index >= sequence_length this
             of true => SOME (lexer, {start=start, length=index})
              | false => let val cur_char = sequence_sub (this, index)
                         in case lexer_next_char lexer of
                                NONE => NONE
                              | SOME (lexer: Lexer, new_char) =>
                                case new_char = cur_char
                                 of true => helper lexer (index + 1)
                                  | false => NONE
                         end
    in helper lexer 0
    end

fun generate_try_lex_keyword (token_type: TokenType, keyword: char sequence)
    : (Lexer -> (Lexer * Token) option)
    = fn lexer: Lexer =>
         case lexer_next_charseq_is (lexer, keyword) of
             NONE => NONE
           | SOME (lexer, indices) =>
             SOME (lexer, make_token (token_type, indices))

val try_lex_segment_marker =
    generate_try_lex_keyword (SegmentMarker, segment_marker_str)
val try_lex_macrodef = generate_try_lex_keyword (MacroDef, macrodef_str)
val try_lex_macroend = generate_try_lex_keyword (MacroEnd, macroend_str)

datatype with_rules_do_what = Recurse | ReturnPrevious |
                              ReturnCurrent | ReturnNone
fun generate_try_lex_with_rules
        (none_case: Lexer * int -> Lexer * int,
         some_case: Lexer * char * int -> with_rules_do_what,
         validate: Lexer * char sequence * seq_spec -> (Lexer * Token) option)
    : Lexer -> (Lexer * Token) option =
    fn lexer => 
       let fun helper (lexer: Lexer) (acc: int): (Lexer * int) option =
               case lexer_next_char lexer of
                   NONE => SOME (none_case (lexer, acc))
                 | SOME (new_lexer, character) =>
                   case some_case (new_lexer, character, acc) of
                       Recurse => helper new_lexer (acc + 1)
                     | ReturnPrevious => SOME (lexer, acc)
                     | ReturnCurrent => SOME (new_lexer, acc+1)
                     | ReturnNone =>  NONE
       in case (helper lexer 0) of
              SOME (_, 0) => NONE
            | SOME (new_lexer, i) =>
              validate (new_lexer,
                        lexer_get_subseq (lexer, lexer_get_index lexer, i),
                        {start=lexer_get_index lexer,
                         length=i})
            | NONE => NONE
       end

(* this sucks, make it better later *)
val try_lex_int_mode =
    generate_try_lex_with_rules
        (fn (lexer, acc) => (lexer, acc),
         fn (lexer, character, acc) =>
            case (acc, character, Char.isSpace character)
             of (0, #"&", false) => Recurse
              | (0, _, false) => ReturnNone
              | (_, _, false) => Recurse
              | (_, _, true) => ReturnPrevious,
         fn (lexer, seq, seq_spec) =>
            let val int_mode_seqs = table_make (
                        sequence_eq,
                        [(int_mode_u8_str , U8),
                         (int_mode_u16_str, U16),
                         (int_mode_u32_str, U32),
                         (int_mode_u64_str, U64),
                         (int_mode_i8_str, I8),
                         (int_mode_i16_str, I16),
                         (int_mode_i32_str, I32),
                         (int_mode_i64_str, I64)])
            in case table_access (int_mode_seqs, seq)
                of SOME (mode) =>
                   SOME (lexer, make_token(IntMode(mode), seq_spec))
                 | NONE => NONE
            end)

val try_lex_string = generate_try_lex_with_rules
                         (fn (lexer, acc) => (lexer, acc),
                          fn (lexer, character, acc) =>
                             case (acc, character)
                              of (0, #"\"") => Recurse
                               | (0, _) => ReturnPrevious
                               | (_, #"\"") => ReturnCurrent
                               | (_, _) => Recurse,
                          fn (lexer, seq, {start=start, length=length}) =>
                             SOME (lexer,
                                   make_token
                                       (StringLiteral,
                                        {start=start + 1, length=length - 2})))

fun char_can_be_id (c, i): bool =
    (case i
      of 0 => Char.isAlpha c
       | _ => Char.isAlpha c orelse c = #"_" orelse Char.isDigit c)

val try_lex_labeldef = generate_try_lex_with_rules
                           (fn (lexer, acc) => (lexer, acc),
                            fn (lexer, character, acc) =>
                               case (char_can_be_id (character, acc), character)
                                of (true, _) => Recurse
                                 | (false, #":") => ReturnCurrent
                                 | (false, _) => ReturnNone,
                            fn (lexer, seq, {start=start, length=length}) =>
                               if (sequence_sub (seq, length - 1) = #":")
                               then SOME (lexer,
                                          make_token(LabelDef,
                                                     {start=start,
                                                      length=length - 1}))
                               else NONE)

exception IllegalInteger
val try_lex_integer = generate_try_lex_with_rules
                          (fn (lexer, acc) => (lexer, acc),
                           fn (lexer, character, acc) =>
                              case Char.isDigit character
                               of true => Recurse
                                | false => ReturnPrevious,
                           fn (lexer, seq, seq_spec) =>
                              SOME (lexer,
                                    make_token (IntegerLiteral, seq_spec)))

val try_lex_id =
    generate_try_lex_with_rules
        (fn (lexer, acc) => (lexer, acc),
         fn (lexer, character, acc) => if char_can_be_id (character, acc)
                                       then Recurse
                                       else ReturnPrevious,
         fn (lexer, seq, seq_spec) =>
            SOME (lexer, make_token(Identifier,
                                    seq_spec)))

fun lexer_skip_whitespace (lexer: Lexer): Lexer option =
    let fun helper (lexer: Lexer): Lexer option =
            case lexer_next_char lexer of
                NONE => NONE
              | SOME (new_lexer, character) => if Char.isSpace character
                                               then helper new_lexer
                                               else SOME lexer
        val curr_char = lexer_current_char lexer
    in
        case curr_char of
            NONE => NONE
          | SOME character => 
            if Char.isSpace character
            then helper lexer
            else SOME lexer
    end


exception UnrecognizedToken
fun lexer_next_token (lexer: Lexer): (Lexer * Token) option =
    let val tries = sequence_of_list [try_lex_segment_marker,
                                      try_lex_macrodef,
                                      try_lex_macroend,
                                      try_lex_string,
                                      try_lex_int_mode,
                                      try_lex_labeldef,
                                      try_lex_id,
                                      try_lex_integer]
        val n_tries = (sequence_length tries) - 1
        fun get_fn (index: int): (Lexer -> (Lexer * Token) option) =
            sequence_sub (tries, index)
        fun try_lex_helper (lexer: Lexer) (n: int): (Lexer * Token) option =
            if n > n_tries then NONE
            else case get_fn n (lexer) of
                     NONE => try_lex_helper lexer (n + 1)
                   | SOME (lexer, token) => SOME (lexer, token)
        val maybe_lexer_ws_skipped = lexer_skip_whitespace lexer
    in
        case maybe_lexer_ws_skipped of
            NONE => NONE
          | SOME (lexer_ws_skipped) => try_lex_helper lexer_ws_skipped 0
    end

(*
fun get_all_tokens (lexer: Lexer): unit =
    let fun helper lexer (tokens_so_far: Token sequence) (max: int): Token sequence = 
            if max < 1 then tokens_so_far
            else case lexer_next_token lexer of
                     NONE => tokens_so_far
                   | SOME (new_lexer, token) =>
                     helper new_lexer
                            (sequence_append (tokens_so_far, token))
                            (max - 1)
    in app (fn a =>
               print ("TOKEN: " ^
                      sequence_to_string (lexer_get_token_substr (lexer, a)) ^
                      (case (#token_type a)
                        of Identifier => ", type = Identifier"
                         | IntegerLiteral => ", type = IntegerLiteral"
                         | StringLiteral => ", type = StringLiteral"
                         | IntMode (i) => ", type = IntMode"
                         | LabelDef => ", type = LabelDef"
                         | MacroDef => ", type = MacroDef"
                         | MacroEnd => ", type = MacroEnd"
                         | SegmentMarker => ", type = SegmentMarker"
                         | EndOfFile => ", type = EndOfFile")
                      ^ "\n"))
           (sequence_to_list (helper lexer (sequence_of_list []) 500)) end
        
val my_lexer1 =
    (lexer_of_string
         (slurp_file "ex.stcass"))
val _ = get_all_tokens my_lexer1
*)


fun lexer_next_token_str (lexer: Lexer)
    : (Lexer * TokenType * char sequence) option =
    case lexer_next_token lexer
     of NONE => NONE
      | SOME (new_lexer, token) =>
        SOME (new_lexer, #token_type token,
              lexer_get_token_substr (new_lexer, token))

type 'a Fifo = 'a sequence
fun fifo_of_list (list: 'a list): 'a Fifo =
    sequence_of_list list
fun fifo_of_list_reverse (list: 'a list): 'a Fifo =
    sequence_reverse (sequence_of_list list)
fun fifo_length (fifo: 'a Fifo): int =
    sequence_length fifo
fun fifo_push (fifo: 'a Fifo, element: 'a): 'a Fifo =
    sequence_append (fifo, element)
fun fifo_concat (fifo: 'a Fifo, seq: 'a sequence): 'a Fifo =
    sequence_concat (fifo, seq)
fun fifo_concat_reverse (fifo: 'a Fifo, seq: 'a sequence): 'a Fifo =
    sequence_concat (fifo, sequence_reverse seq)
fun fifo_pop (fifo: 'a Fifo): ('a Fifo * 'a) =
    (sequence_take (fifo, fifo_length fifo - 1),
     sequence_sub  (fifo, fifo_length fifo - 1))
type token_queue = (Lexer * Token Fifo)
fun token_queue_of_lexer (lexer: Lexer)
    : token_queue =
    (lexer, fifo_of_list [])
fun token_queue_push_tokens ((lexer, fifo): token_queue, seq: Token sequence)
    : token_queue =
    (lexer, fifo_concat_reverse (fifo, seq))
fun token_queue_pop ((lexer, fifo): token_queue): (token_queue * Token) option =
    case fifo_length fifo
     of 0 => (case lexer_next_token lexer
               of SOME (new_lexer, element) => SOME ((new_lexer, fifo), element)
                | NONE => NONE)
      | _ => let val (new_fifo, element) = (fifo_pop fifo)
             in SOME ((lexer, new_fifo), element) end
fun token_queue_pop_str ((lexer, fifo): token_queue):
    (token_queue * Token * (TokenType * char sequence)) option =
    case fifo_length fifo
     of 0 => (case lexer_next_token lexer
               of SOME (new_lexer, token) =>
                  SOME ((new_lexer, fifo), token,
                        (#token_type token,
                         lexer_get_token_substr (new_lexer, token)))
                | NONE => NONE)
      | _ => let val (new_fifo, element) = (fifo_pop fifo)
             in SOME ((lexer, new_fifo),
                      element,
                      (#token_type element,
                       lexer_get_token_substr (lexer, element))) end

val my_fifo =  fifo_of_list_reverse [1, 2, 3, 4]
val my_fifo2 = fifo_concat_reverse (my_fifo, sequence_of_list [10,20,30]);
fun pop_all (fifo: int Fifo): unit =
    let fun helper index fifo =
            if fifo_length fifo = 0 then ()
            else let val (new_fifo, element) = fifo_pop fifo
                 in print ((Int.toString element) ^ "\n");
                    helper (index - 1) new_fifo
                 end
    in helper 0 fifo end

type macro = Token sequence
type named_macro_table = (char sequence, macro) table
fun get_macro (macros: named_macro_table, seq: char sequence): macro option =
    table_access (macros, seq)
fun add_macro (macros: named_macro_table, seq: char sequence, macro: macro)
    : named_macro_table =
    table_add (macros, seq, macro)

fun size_in_bytes_of_intmode U64 = 8
  | size_in_bytes_of_intmode I64 = 8
  | size_in_bytes_of_intmode U32 = 4
  | size_in_bytes_of_intmode I32 = 4
  | size_in_bytes_of_intmode U16 = 2
  | size_in_bytes_of_intmode I16 = 2
  | size_in_bytes_of_intmode U8 = 1
  | size_in_bytes_of_intmode I8 = 1

type color = {r:int, g:int, b:int}

type MacrolessContext = token_queue * named_macro_table * int

fun macroless_context_of_token_queue (tq: token_queue)
    : MacrolessContext =
    (tq, table_make (sequence_eq, []), 8)

datatype MacrolessNode = MlInteger of LargeInt.int * int
                       | MlString of char sequence * int
                       | MlLabelCall of char sequence * int
                       | MlLabelDef of char sequence
                       | MlSegment of char sequence

exception Unimplemented
exception IllegalMacroEnd
exception ExpectedMacroName
exception ExpectedMacroEnd
exception EndOfFileInMacro
exception ExpectedSegmentMarkerName

fun set_macro ((tq, macros, size): MacrolessContext)
    : MacrolessContext = 
    let fun helper (tq: token_queue, macros_so_far: Token sequence)
            : (token_queue * Token sequence) =
            case token_queue_pop_str tq
             of SOME (new_tq, token, (MacroEnd, _)) => (new_tq, macros_so_far)
              | SOME (new_tq, token, (EndOfFile, _)) => raise EndOfFileInMacro
              | SOME (new_tq, token, _) =>
                helper (new_tq, sequence_append (macros_so_far, token))
              | NONE => raise ExpectedMacroEnd
    in
        case token_queue_pop_str tq
         of SOME (new_tq, token, (Identifier, name)) =>
            let val (new_tq, new_macro) = helper (new_tq, sequence_of_list [])
            in (new_tq, add_macro (macros, name, new_macro), size) end
          | _ => raise ExpectedMacroName
    end
        

fun get_macroless_node ((tq, macros, size): MacrolessContext):
    (MacrolessContext * (MacrolessNode * seq_spec)) option =
    case token_queue_pop_str tq
     of SOME (new_tq, token, (MacroDef, macroname)) => 
        get_macroless_node (set_macro (new_tq, macros, size))
      | SOME (new_tq, token, (MacroEnd, _)) => raise IllegalMacroEnd
      | SOME (new_tq, token, (IntegerLiteral, seq)) =>
        SOME ((new_tq, macros, size),
              (MlInteger (sequence_to_large_integer seq, size), #indices token))
      | SOME (new_tq, token, (LabelDef, seq)) => 
        SOME ((new_tq, macros, size),
              (MlLabelDef seq, #indices token))
      | SOME (new_tq, token, (Identifier, seq)) => 
        (case get_macro (macros, seq)
          of SOME (macro) => get_macroless_node
                                 (token_queue_push_tokens (new_tq, macro),
                                  macros, size)
           | NONE =>
             SOME ((new_tq, macros, size), (MlLabelCall (seq, size),
                                            #indices token)))
      | SOME (new_tq, token, (SegmentMarker, seq)) =>
        (case token_queue_pop_str new_tq
          of SOME (new_new_tq, new_token, (Identifier, seq)) =>
             SOME ((new_new_tq, macros, size),
                   (MlSegment (seq), #indices new_token))
           | _ => raise ExpectedSegmentMarkerName)
      | SOME (new_tq, token, (IntMode(mode), seq)) =>
        get_macroless_node (new_tq, macros, size_in_bytes_of_intmode mode)
      | SOME (new_tq, token, (EndOfFile, seq)) => NONE
      | SOME (new_tq, token, (StringLiteral, seq)) =>
        SOME ((new_tq, macros, size),
              (MlString (seq, sequence_length seq), #indices token))
      | NONE => NONE

(*"%macrodef push &u64 4 %macroend push &u64 1 2 3 4 5"))
 *)

fun all_macroless_nodes (ctx: MacrolessContext): MacrolessNode sequence =
    let fun helper (ctx: MacrolessContext, seq_so_far: MacrolessNode sequence)
            : MacrolessNode sequence =
            case get_macroless_node ctx
             of SOME (new_ctx, (node, _)) =>
                helper (new_ctx, sequence_append (seq_so_far, node))
              | NONE => seq_so_far
    in helper (ctx, sequence_of_list []) end


type LabelDefs = (char sequence, int) table
fun labeldefs_to_string (ld: LabelDefs) =
    let val seq = #seq ld
        fun loop (index, acc) =
            if index < sequence_length seq
            then loop (index + 1,
                       String.concat
                           [acc,
                            "  this guy '",
                            sequence_to_string (#1 (sequence_sub (seq, index))),
                            "' is ",
                            Int.toString (#2 (sequence_sub (seq, index))),
                            "\n"])
            else acc
    in loop (0, "") end

type SegmentlessContext = (LabelDefs * int)
fun segmentlesscontext_get_size (ctx: SegmentlessContext) = #2 ctx
fun add_label (ld: LabelDefs, name: char sequence, location: int)
    : LabelDefs =
    table_add (ld, name, location)
fun get_label (ld: LabelDefs, name: char sequence): int option =
    table_access (ld, name)
fun segmentlesscontext_add_label
        (ctx: SegmentlessContext, name: char sequence, location: int)
    : LabelDefs =
    table_add (#1 ctx, name, location)
datatype SegmentlessNode = SlInteger of LargeInt.int * int
                         | SlString of char sequence * int
                         | SlLabelCall of char sequence * int
type Segment = SegmentlessNode sequence * char sequence

fun segment_print ((seq, name): Segment): unit =
    (print ("SegmentName: " ^ (sequence_to_string(name)) ^ "\n");
     sequence_app
         (seq,
          (fn (sn: SegmentlessNode) =>
              case sn
               of SlInteger(i, sz) =>
                  print ("Integer: '" ^
                         LargeInt.toString(i) ^
                         "' size: " ^ Int.toString(sz) ^
                         "\n")
                | SlString(s, sz) =>
                  print ("String: '" ^
                         sequence_to_string(s) ^
                         "' size: " ^ Int.toString(sz) ^
                         "\n")
                | SlLabelCall(lc, sz) =>
                  print ("LabelCall: '" ^
                         sequence_to_string(lc) ^
                         "' size: " ^ Int.toString(sz) ^
                         "\n"))))

exception ExpectedSegment
fun get_segment (ctx: MacrolessContext, ld: LabelDefs)
    : (MacrolessContext * Segment * LabelDefs) option =
    let fun helper (ctx: MacrolessContext,
                    seq: SegmentlessNode sequence,
                    (labeldefs, size): SegmentlessContext)
            : (MacrolessContext * SegmentlessNode sequence *
               LabelDefs) =
            case get_macroless_node ctx
             of SOME (new_ctx, (MlInteger (inum, tsz), _)) =>
                helper (new_ctx, sequence_append (seq, SlInteger (inum, tsz)),
                        (labeldefs, size + tsz))
              | SOME (new_ctx, (MlString (str, tsz), _)) =>
                helper (new_ctx, sequence_append (seq, SlString (str, tsz)),
                        (labeldefs, size + tsz))
              | SOME (new_ctx, (MlLabelCall (str, tsz), _)) =>
                helper (new_ctx, sequence_append (seq, SlLabelCall (str, tsz)),
                        (labeldefs, size + tsz))
              | SOME (new_ctx, (MlSegment (str), _)) =>
                (ctx, seq, labeldefs)
              | SOME (new_ctx, (MlLabelDef (str), _)) =>
                helper (new_ctx, seq, (add_label (labeldefs, str, size), size))
              | NONE => (ctx, seq, labeldefs)
    in
        case get_macroless_node ctx
         of SOME (new_ctx, (MlSegment(name), _)) =>
            SOME (case (helper (new_ctx, sequence_of_list [], (ld, 0)))
                   of (new_new_ctx, snseq, new_ld) =>
                      (new_new_ctx, (snseq, name), new_ld))
          | NONE => NONE
          | _ => raise ExpectedSegment
    end

exception IntTooBig
fun int_to_little_endian_sequence (word: LargeInt.int, bytesize: int)
    : Word8.word sequence =
    let val largeword = LargeWord.fromLargeInt word
        fun loop (index: int) (acc: Word8.word sequence) =
            if index >= bytesize 
            then acc
            else
                let val shift = Word.fromInt (index * 8)
                    val shifted = LargeWord.>> (largeword, shift)
                    val anded = LargeWord.andb (shifted, 0wxff)
                    val asbyte = Word8.fromLarge anded
                in loop (index + 1) (sequence_append (acc, asbyte)) end
    in
        if largeword >
           (LargeWord.<< (LargeWord.fromInt 1,
                          Word.fromInt (8 * bytesize)) - 0w1)
        then raise IntTooBig
        else loop 0 (sequence_of_list [])
    end

fun word8sequence_to_word8vector (ws: Word8.word sequence): Word8Vector.vector =
    Word8Vector.fromList (sequence_to_list ws)

exception UnidentifiedIdentifier of string
fun segmentless_node_to_bytes(node: SegmentlessNode, labels: LabelDefs)
    : Word8.word sequence =
    case node
     of SlInteger (num, s) => int_to_little_endian_sequence (num, s)
      | SlString (str, s) => sequence_map (str, Byte.charToByte)
      | SlLabelCall (lc, s) =>
        case get_label (labels, lc)
         of SOME location =>
            int_to_little_endian_sequence (LargeInt.fromInt location, s)
          | NONE =>
             raise UnidentifiedIdentifier (sequence_to_string lc)

fun segmentless_nodes_to_bytes(seq: SegmentlessNode sequence, labels: LabelDefs)
    : Word8.word sequence =
    let fun loop (index, acc): Word8.word sequence =
            if (index < sequence_length seq)
            then let val new_node = segmentless_node_to_bytes
                                        (sequence_sub (seq, index), labels)
                 in loop (index + 1, sequence_concat (acc, new_node)) end
            else acc
    in loop (0, sequence_of_list []) end

fun print_all_segments (ctx: MacrolessContext): unit =
    let fun loop (ctx: MacrolessContext): unit =
            case get_segment (ctx, table_make (sequence_eq, []))
             of SOME (new_ctx, seg, _) => (segment_print(seg);
                                           loop (new_ctx))
              | _ => ()
    in loop ctx end

fun get_all_segments_in_bytesequence (ctx: MacrolessContext) =
    let fun loop (ctx: MacrolessContext, ld: LabelDefs, acc) =
            case get_segment (ctx, ld)
             of SOME (new_ctx, (seq, name), new_ld) =>
                loop (new_ctx, new_ld, sequence_append (acc, (name, seq)))
              | NONE => (ld, acc)
        val (ld, seqs) = loop (ctx, table_make (sequence_eq, []),
                               sequence_of_list [])
        fun loop2 (index, acc) =
            if index >= sequence_length seqs
            then acc
            else let val (name, seq) = (sequence_sub (seqs, index))
                     val seq_in_bytes = segmentless_nodes_to_bytes (seq, ld)
                     val new_acc = table_add (acc, name, seq_in_bytes)
                 in loop2 (index + 1, new_acc) end
        val ret = loop2 (0, table_make (sequence_eq, []))
    in ret end

fun word8vectorprint (w8v: Word8Vector.vector) =
    (Word8Vector.appi (fn (i, a) =>
                          (print (Word8.toString a ^ " ");
                           if i mod 16 = 15 
                           then print "\n"
                           else ()))
                      w8v;
     print "\n")

fun val_of_list(ls: 'a option list): 'a list option =
    let fun loop ls acc =
            case ls
             of SOME(elem) :: tl => loop tl (elem :: acc)
              | NONE :: tl => NONE
              | [] => SOME (List.rev acc)
    in loop ls [] end

(*
conforming to this table
+--------------+--------+ 
| STC          | 3      |
+--------------+--------+
| data length  | 4      |
+--------------+--------+
| data         | dl     |
+--------------+--------+
| code length  | 4      |
+--------------+--------+
| code         | cl     |
+--------------+--------+
| stack length | sl     |
+--------------+--------+
| stack        | sl * 8 |
+--------------+--------+
*)
exception MissingSomeSegments
fun output_to_file ({cmp=cmp, length=len, seq=segments}:
                    (char sequence, Word8.word sequence) table, what_file) =
    let val transformed_seq =
            sequence_map (segments,
                          fn (name, segment) =>
                             (sequence_to_string name,
                              word8sequence_to_word8vector segment))
        val transformed_table:
            (string, Word8Vector.vector) table =
            {cmp=fn (a, b) => String.compare (a, b) = EQUAL,
             length=len,seq=transformed_seq}
        val mysegs: (Word8Vector.vector) list option =
            val_of_list [table_access (transformed_table, "data"),
                         table_access (transformed_table, "code"),
                         table_access (transformed_table, "stack")]
    in
        case mysegs
         of SOME (data :: code :: stack :: []) =>
            let val strm = BinIO.openOut what_file;
                val stc = Word8Vector.fromList
                              (map Byte.charToByte (String.explode "stc"))
                fun getveclen sg =
                    word8sequence_to_word8vector
                        (int_to_little_endian_sequence
                             (LargeInt.fromInt
                                  (Word8Vector.length sg),
                              4))
                val datalength =  getveclen data
                val codelength =  getveclen code
                val stacklength = getveclen stack
            in 
               BinIO.output (strm,
                             Word8Vector.concat
                                 [stc,
                                  datalength,
                                  data,
                                  codelength,
                                  code,
                                  stacklength,
                                  stack]);
               BinIO.closeOut strm
            end
          | _ => raise MissingSomeSegments
          (*| NONE => raise MissingSomeSegments*)
    end
    (*Word8Vector.fromList (sequence_to_list segments)*)

exception ArgsAreWrong
fun main (args: string list): unit =
    let type config = (string option * string option)
        fun arg_process (args: string list, (previn, prevout): config)
            : (string * string)=
            case (args, previn, prevout)
             of ([], NONE, NONE) => raise ArgsAreWrong
              | ([], SOME in_file, SOME out_file) =>
                (in_file, out_file)
              | ("-o" :: outname :: tl, _, NONE) =>
                arg_process (tl, (previn, SOME outname))
              | (inname :: tl, NONE, _) =>
                arg_process (tl, (SOME inname, prevout))
              | _ => raise ArgsAreWrong
        val (in_file, out_file) = arg_process (args, (NONE, NONE))
        val lexer = lexer_of_string (slurp_file in_file)
        val tq = token_queue_of_lexer lexer
        val mlctx = macroless_context_of_token_queue tq
        val segs = get_all_segments_in_bytesequence mlctx
    in
        output_to_file (segs, out_file)
    end

val _ = main (CommandLine.arguments ())
        
