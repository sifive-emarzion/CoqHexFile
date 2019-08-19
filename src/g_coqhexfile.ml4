DECLARE PLUGIN "coqhexfile_plugin"

open Stdarg

(*** Printing messages ***)

(*
 * This defines a command that prints HelloWorld.
 * Note that Feedback.msg_notice can be used to print messages.
 *)
VERNAC COMMAND EXTEND HexLoad CLASSIFIED AS SIDEFF
| [ "HexLoad" string(filename) "as" ident(i) ] -> [
  let bs = Coqhexfile_main.read_bytes_from_file filename in
  let e = Coqhexfile_main.coq_bytes_of_bytes bs in
  Coqhexfile_main.declare_term i e ]
END;;
