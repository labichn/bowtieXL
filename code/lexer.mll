(*
{ header }
let ident = regexp …
rule entrypoint [arg1… argn] =
  parse regexp { action }
      | …
      | regexp { action }
and entrypoint [arg1… argn] =
  parse …
and …
{ trailer }
*)

{
  open Lexing
  open Parser
  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- 
      { pos with
          Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
          Lexing.pos_bol = pos.Lexing.pos_cnum;
      }

}

let int = '-'? ['0'-'9'] ['0'-'9']*
let noteol = [^'\n']
let eol = ['\n']
let char = ['!' '"' '#' '$' '%' '&' '\'' '(' ')' '*' '+' ',' '-' '.' '/' '0'-'9' ':' ';' '<' '=' '>' '?' '@' 'A'-'Z' '[' '\\' ']' '^' '_' '`' 'a'-'z' '{' '|' '}' '~']
let word = char+
let string = (word | ' ' | '\t')+

rule token = parse
| [' ' '\t']  { token lexbuf }
| eol         { incr_linenum lexbuf; token lexbuf } 
| '@'         { AMP }
| ';'         { SEMICOLON }
| '>'         { GT }
| string as s { STR s }
| eof         { EOF }
