#use "topfind"

(* #require "core.top";; *)
(* #require "ppx_jane";; *)
(* open Base;; *)
#require "lambda-term"

#edit_mode_vi;;

Lwt.map (fun term -> LTerm.set_escape_time term 0.01) (Lazy.force LTerm.stdout)

let toggle_semi =
  let semi = ref true in
  let original = !UTop.parse_toplevel_phrase in
  let no_semi str eos_is_error = original (str ^ ";;") eos_is_error in
  fun () ->
    UTop.parse_toplevel_phrase := if !semi then no_semi else original;
    semi := not !semi;
    let open LTerm_text in
    UTop.prompt
    := [ B_fg (if !semi then LTerm_style.lgreen else LTerm_style.index 214); S "# " ]
       |> eval
       |> React.S.const
;;

toggle_semi ();
LTerm_read_line.bind
  [ { control = false; meta = false; shift = false; code = F6 } ]
  [ LTerm_read_line.Edit (Custom toggle_semi) ]
