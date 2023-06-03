(*
  Copyright 2023 Souchet Ferdinand

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
  documentation files (the “Software”), to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit
  persons to whom the Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
  WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
  OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)


let exec_cin context cin (fn: Lambda.expr -> unit) = 
  let lexbuf = Lexing.from_channel cin in
  try
    while true do
      let s = Lparser.input Llexer.lambda lexbuf in
      fn @@ context#exec s;
    done
  with
  | End_of_file         -> ()
;;


let exec_str context str (fn: Lambda.expr -> unit) =
  try
    let lexbuf = Lexing.from_string str in
    while true do
      let s = Lparser.input Llexer.lambda lexbuf in
      fn @@ context#exec s;
    done
  with
  | End_of_file -> ()
;;



let exec_file context file =
  Printf.printf "File %s:\n\n" file;
  exec_cin context (open_in file) (fun _ -> ());
  context#print ();
  print_newline ()
;;


let exec_stdin context =
  try
    while true do
      Printf.printf "\x1b[32;1m> \x1b[m"; flush stdout;
      let input = (read_line ())^"\n" in
      try exec_str context input (fun e -> Lambda.print_expr e; print_newline ())
      with
      | Lambda.UndefinedVariable(v) -> Printf.printf "\x1b[31mUndefined variable: %s\x1b[m\n" v
    done
  with
  | End_of_file         -> print_string "\x1b[M\n"; flush stdout; exit 0
;;



let main () =
  let lamb_ctx = new Lambda.context in
  for i = 1 to Array.length Sys.argv - 1 do
    exec_file lamb_ctx Sys.argv.(i)
  done;
  exec_stdin lamb_ctx
;;


let _ = Printexc.print main ();;