type binop = Eq | Ne | Lt | Le | Gt | Ge

type expr =
  | Path of string list
  | Lit of Yojson.Safe.t
  | Param of string
  | Not of expr
  | Bin of binop * expr * expr
  | And of expr * expr
  | Or of expr * expr
  | In of expr * expr list
  | Between of expr * expr * expr
  | Call of string * expr list

type select_item = { item : expr; alias : string option }
type select = All | Items of select_item list | Value of expr
type order = Asc | Desc

type t = {
  select : select;
  alias : string;
  where : expr option;
  order_by : (expr * order) list;
  top : int option;
  offset : int;
  limit : int option;
}
