{
open Parser

exception Lex_error of string
}

let ws = [' ' '\t' '\r' '\n']+
let digit = ['0'-'9']
let int = digit+
let float =
  digit+ '.' digit* (['e' 'E'] ['+' '-']? digit+)?
  | digit+ ['e' 'E'] ['+' '-']? digit+
  | '.' digit+ (['e' 'E'] ['+' '-']? digit+)?
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  | ws { token lexbuf }
  | int as i { INT (int_of_string i) }
  | float as f { FLOAT (float_of_string f) }
  | '@' (ident as id) { PARAM id }
  | ident as id {
      match String.uppercase_ascii id with
      | "SELECT" -> SELECT
      | "FROM" -> FROM
      | "WHERE" -> WHERE
      | "AND" -> AND
      | "OR" -> OR
      | "NOT" -> NOT
      | "IN" -> IN
      | "BETWEEN" -> BETWEEN
      | "AS" -> AS
      | "ORDER" -> ORDER
      | "BY" -> BY
      | "ASC" -> ASC
      | "DESC" -> DESC
      | "TOP" -> TOP
      | "OFFSET" -> OFFSET
      | "LIMIT" -> LIMIT
      | "VALUE" -> VALUE
      | "TRUE" -> BOOL true
      | "FALSE" -> BOOL false
      | "NULL" -> NULL
      | "UNDEFINED" -> UNDEFINED
      | _ -> IDENT id }
  | '\'' { STRING (read_string '\'' (Buffer.create 16) lexbuf) }
  | '"' { STRING (read_string '"' (Buffer.create 16) lexbuf) }
  | '=' { EQ }
  | "!=" { NE }
  | "<=" { LE }
  | ">=" { GE }
  | '<' { LT }
  | '>' { GT }
  | "<>" { NE }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | '.' { DOT }
  | '[' { LBRACK }
  | ']' { RBRACKET }
  | '*' { STAR }
  | '-' { MINUS }
  | eof { EOF }
  | _ as c { raise (Lex_error (Printf.sprintf "unexpected character %C" c)) }

and read_string quote buf = parse
  | "''" {
      if quote = '\'' then Buffer.add_char buf '\''
      else Buffer.add_string buf "''";
      read_string quote buf lexbuf }
  | "\"\"" {
      if quote = '"' then Buffer.add_char buf '"'
      else Buffer.add_string buf "\"\"";
      read_string quote buf lexbuf }
  | '\'' {
      if quote = '\'' then Buffer.contents buf
      else (
        Buffer.add_char buf '\'';
        read_string quote buf lexbuf) }
  | "\\'" { Buffer.add_char buf '\''; read_string quote buf lexbuf }
  | "\\\"" { Buffer.add_char buf '"'; read_string quote buf lexbuf }
  | "\\\\" { Buffer.add_char buf '\\'; read_string quote buf lexbuf }
  | "\\n" { Buffer.add_char buf '\n'; read_string quote buf lexbuf }
  | "\\t" { Buffer.add_char buf '\t'; read_string quote buf lexbuf }
  | eof { raise (Lex_error "unterminated string") }
  | _ as c { Buffer.add_char buf c; read_string quote buf lexbuf }
