type state = {
  mutable output_file : string option;
  mutable input_file : string option;
}

let state = {
  output_file = None;
  input_file = None;
}

let set_output_file path = state.output_file <- Some(path)
let output_file () = state.output_file

let set_input_file path = state.input_file <- Some(path)
let input_file () = state.input_file
