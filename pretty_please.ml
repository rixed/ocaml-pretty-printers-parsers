let forever f =
  while true do f ()  done ;
  assert false

let read_whole_file ic =
  let buf = Buffer.create 10000 in
  try forever (fun () ->
    input_line ic |> Buffer.add_string buf ;
    Buffer.add_char buf '\n')
  with End_of_file ->
    Buffer.contents buf

let () =
  read_whole_file stdin |>
  PPP_prettify.prettify |>
  print_string
