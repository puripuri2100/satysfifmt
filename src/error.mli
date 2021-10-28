open Range

exception Parser_error of string
exception LexError of Range.t * string
exception NoInputMainFileName

val error_msg : (unit -> unit) -> unit
