%{
open Lexing
     (*let _ = Parsing.set_trace true*)
%}

%token SEMICOLON GT EOF NEWLINE AMP
%token <string> STR WORD

%start fasta
%start fastq

%type <(string * string * string * string) list> fasta
%type <string * string> fastq
%%

fasta:
| reads EOF { $1 }
| reads { $1 }
;

reads:
| rev_reads { List.rev $1 }
;

rev_reads:
| { [] }
| rev_reads read { $2 :: $1 }
;

read:
| STR STR STR STR { ($1, $2, $3, $4) }
;

fastq:
| STR seq EOF { ($1, $2) }
| STR seq { ($1, $2) }
;

seq:
| rev_seq { let string_of_list ?(sep="") ?(border=(fun s->s)) soe l =
              if List.length l > 0 then
                let elts = List.fold_right (fun elt a -> (soe elt)^sep^a) l "" in
                border (String.sub elts 0 ((String.length elts)-(String.length sep)))
              else border ""
            in
            string_of_list (fun s->s) (List.rev $1) }
;

rev_seq:
| { [] }
| rev_seq STR { $2 :: $1 }
;
            
