%{
open Ast
%}

%token SELECT FROM WHERE AND OR NOT IN BETWEEN AS ORDER BY ASC DESC
%token TOP OFFSET LIMIT VALUE
%token NULL UNDEFINED
%token <bool> BOOL
%token <string> IDENT PARAM STRING
%token <int> INT
%token <float> FLOAT
%token EQ NE LT LE GT GE
%token LPAREN RPAREN COMMA DOT LBRACK RBRACKET STAR MINUS
%token EOF

%start <Ast.t> query

%%

query:
  | SELECT top = option(preceded(TOP, INT)) items = select_list
    FROM alias = from_alias
    wh = option(preceded(WHERE, expr))
    ord = option(order_by)
    off = option(preceded(OFFSET, INT))
    lim = option(preceded(LIMIT, INT))
    EOF
    { { select = items;
        alias;
        where = wh;
        order_by = Option.value ord ~default:[];
        top;
        offset = Option.value off ~default:0;
        limit = lim } }

from_alias:
  | first = IDENT rest = option(preceded(option(AS), IDENT))
    { Option.value rest ~default:first }

select_list:
  | STAR { All }
  | VALUE e = operand { Value e }
  | items = separated_nonempty_list(COMMA, select_item) { Items items }

select_item:
  | e = operand a = option(preceded(option(AS), IDENT))
    { { Ast.item = e; Ast.alias = a } }

order_by:
  | ORDER BY l = separated_nonempty_list(COMMA, order_item) { l }

order_item:
  | e = operand d = option(dir)
    { (e, match d with Some Ast.Desc -> Ast.Desc | _ -> Ast.Asc) }

dir:
  | ASC { Ast.Asc }
  | DESC { Ast.Desc }

expr:
  | e = or_expr { e }

or_expr:
  | l = or_expr OR r = and_expr { Or (l, r) }
  | e = and_expr { e }

and_expr:
  | l = and_expr AND r = factor { And (l, r) }
  | e = factor { e }

factor:
  | NOT e = factor { Not e }
  | e = comparison { e }

comparison:
  | l = operand suffix = comp_suffix
    { match suffix with
      | `Bin (op, r) -> Bin (op, l, r)
      | `Between (lo, hi) -> Between (l, lo, hi)
      | `In items -> In (l, items)
      | `None -> l }

comp_suffix:
  | EQ r = operand { `Bin (Eq, r) }
  | NE r = operand { `Bin (Ne, r) }
  | LT r = operand { `Bin (Lt, r) }
  | LE r = operand { `Bin (Le, r) }
  | GT r = operand { `Bin (Gt, r) }
  | GE r = operand { `Bin (Ge, r) }
  | BETWEEN lo = operand AND hi = operand { `Between (lo, hi) }
  | IN LPAREN items = separated_nonempty_list(COMMA, expr) RPAREN
    { `In items }
  | { `None }

operand:
  | e = atom { e }
  | LPAREN e = expr RPAREN { e }
  | MINUS e = operand
    { (match e with
       | Lit (`Int i) -> Lit (`Int (-i))
       | Lit (`Float f) -> Lit (`Float (-.f))
       | other -> other) }

atom:
  | id = IDENT segs = path_tail
    { Path (id :: segs) }
  | i = INT { Lit (`Int i) }
  | f = FLOAT { Lit (`Float f) }
  | s = STRING { Lit (`String s) }
  | b = BOOL { Lit (`Bool b) }
  | NULL { Lit `Null }
  | UNDEFINED { Lit `Null }
  | p = PARAM { Param p }
  | f = IDENT LPAREN args = separated_list(COMMA, expr) RPAREN
    { Call (String.uppercase_ascii f, args) }

path_tail:
  | DOT id = IDENT rest = path_tail { id :: rest }
  | LBRACK s = STRING RBRACKET rest = path_tail { s :: rest }
  | { [] }

%%
