
open Str

let contrib = "hex_loader"

exception ReadError of string

let read_bytes txt = split (regexp "[ \n]+") txt

let read_bytes_from_file filename =
  let ch = open_in filename in
  let txt = really_input_string ch (in_channel_length ch) in
  let () = close_in ch in
  read_bytes txt

exception Anomaly of string

let mkConstructor cstr =
    let ref = Coqlib.gen_reference_in_modules contrib [["Coq";"Init";"Datatypes"];["Hexfile";"Byte"]] cstr in
      match ref with
      | Globnames.ConstructRef c -> Constr.mkConstruct c
      | _ -> raise (Anomaly "Could not construct the term")

let mkByte =
  let ref = Coqlib.gen_reference_in_modules contrib [["Coq";"Init";"Datatypes"];["Hexfile";"Byte"]] "byte" in
    match ref with
    | Globnames.IndRef i -> Constr.mkInd i
    | _ -> raise (Anomaly "Could not construct Byte")

let rec coq_bytes_of_bytes = function
  | [] -> Constr.mkApp (mkConstructor "nil" , [| mkByte |])
  | x :: xs ->
    let tail = coq_bytes_of_bytes xs in
    let name = "x" ^ x in
    let head = mkConstructor name in
    Constr.mkApp (mkConstructor "cons" , [| mkByte ; head ; tail |])

let declare_term ident body =
  let sigma = Evd.from_ctx UState.empty in
  let ubinders = Evd.universe_binders sigma in
  let udecl = UState.default_univ_decl in
  let univs = Evd.check_univ_decl ~poly:false sigma udecl in
  let ce = Declare.definition_entry ?types:None ~univs body in
  let k = (Decl_kinds.Global, Flags.is_universe_polymorphism(), Decl_kinds.Definition) in
  let nohook = Lemmas.mk_hook (fun _ x -> x) in
  ignore (DeclareDef.declare_definition ident k ce ubinders [] nohook)
