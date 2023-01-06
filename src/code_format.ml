open Types

let is_string_empty str = String.equal str ""

let stack_to_lst stack = Stack.fold (fun lst value -> value::lst) [] stack
let append_to_stack stack lst =
  let rec sub lst =
    match lst with
    | [] -> ()
    | x::xs -> let () = stack |> Stack.push x in sub xs
  in
  sub lst

(* ブロックコメントを整形する *)
let before_comments_format (ctx:context) (comments:string list) : string list option =
  if is_lst_empty comments then
    None
  else
    if (List.length comments) == 1 then
      Some([ctx.oneline_comment_format (List.hd comments)])
    else
      Some(ctx.block_comment_format ctx comments)



let rec code_format_sub (ctx:context) (rule_with_comment:rule_with_comment) : (string list * bool) =
  match rule_with_comment.rule with
  | AST(ast) -> (
    let v = Stack.create () in
    let () =
      match before_comments_format ctx rule_with_comment.before_comments with
      | Some(lst) -> append_to_stack v lst
      | None -> ()
    in
    let (rule_format_vec, _) = code_format_sub ctx ast in
    let () = append_to_stack v rule_format_vec in
    match rule_with_comment.after_comment with
    | Some(after_comment) -> let () = v |> Stack.push after_comment in (stack_to_lst v, true)
    | None -> (stack_to_lst v, false)
  )
  | Raw(str) -> (
    let v = Stack.create () in
    let () =
      match before_comments_format ctx rule_with_comment.before_comments with
      | Some(lst) -> append_to_stack v lst
      | None -> ()
    in
    match rule_with_comment.after_comment with
    | Some(after_comment) -> (
      let comment = ctx.oneline_comment_format after_comment in
      let () =
        match ctx.list_join_str with
        | Some(joiner) -> v |> Stack.push (str ^ joiner ^ " " ^ comment)
        | None -> v |> Stack.push (str ^ " " ^ comment)
      in
      (stack_to_lst v, true)
    )
    | None -> (
      let () = v |> Stack.push str in
      (stack_to_lst v, false)
    )
  )
  | Paren(open_str, child_rule_with_comment, close_str) -> (
    let (str_lst, is_exist_after_comment) =
      code_format_sub (ctx |> set_list_join_str(None)) child_rule_with_comment
    in
    let v = Stack.create () in
    let () =
      match before_comments_format ctx rule_with_comment.before_comments with
      | Some(lst) -> append_to_stack v lst
      | None -> ()
    in
    if List.length str_lst <= 1 && not is_exist_after_comment then
      match rule_with_comment.after_comment with
      | Some(after_comment) -> (
        let after_comment_str = ctx.oneline_comment_format after_comment in
        let code_str = lst_join "" str_lst in
        let () =
          match ctx.list_join_str with
          | Some(joiner) -> (
            v |> Stack.push (open_str ^ code_str ^ close_str ^ joiner ^ " " ^ after_comment_str)
          )
          | None -> (
            v |> Stack.push (open_str ^ code_str ^ close_str ^ " " ^ after_comment_str)
          )
        in
        (stack_to_lst v, true)
      )
      | None -> (
        let () = v |> Stack.push (open_str ^ lst_join "" str_lst ^ close_str) in
        (stack_to_lst v, false)
      )
    else
      let () = v |> Stack.push open_str in
      let rec sub lst =
        match lst with
        | [] -> ()
        | code_str::xs -> let () = v |> Stack.push code_str in sub xs
      in
      let ctx = ctx |> increment_depth |> set_list_join_str None in
      let (code_str_lst, _) = code_format_sub ctx child_rule_with_comment in
      let () = sub code_str_lst in
      match rule_with_comment.after_comment with
      | Some(after_comment) -> (
        let comment = ctx.oneline_comment_format after_comment in
        let () =
          match ctx.list_join_str with
          | Some(joiner) -> v |> Stack.push (close_str ^ joiner ^ " " ^comment)
          | None -> v |> Stack.push (close_str ^ " " ^ comment)
        in
        (stack_to_lst v, true)
      )
      | None -> let () = v |> Stack.push close_str in (stack_to_lst v, false)
  )
  | List(join, lst) -> break_token_list ctx join rule_with_comment.before_comments lst rule_with_comment.after_comment
  | Column(lst) -> break_token_column ctx rule_with_comment.before_comments lst rule_with_comment.after_comment


(* Listルールをフォーマットする *)
and break_token_list
  (ctx:context)
  (join:string)
  (before_comments: string list)
  (lst : rule_with_comment list)
  (after_comment_opt : string option)
  : (string list * bool) =
  let tab = indent ctx in
  let rec sub lst buf is_oneline_last_comment_exsits =
    match lst with
    | [] -> (false, buf, is_oneline_last_comment_exsits)
    | new_rule_with_comment::xs -> (
      let (code_str_lst, is_last_exists_after_comment) =
        code_format_sub (ctx |> set_list_join_str (Some(join))) new_rule_with_comment
      in
      if
        (* 要素の前のコメントが存在する要素が一つでもあるか、 *)
        not (is_lst_empty new_rule_with_comment.before_comments)
        (* 最後の要素以外の要素で、要素直後のコメントが一つでも存在するか、 *)
        || (List.length xs >= 1 && is_last_exists_after_comment)
        (* 出力結果が複数行のとき *)
        || List.length code_str_lst > 1
      then
        (true, "", is_oneline_last_comment_exsits)
      else
        let code_str = lst_join "" code_str_lst in
        if List.length xs >= 1 then
          sub xs (buf ^ code_str ^ join ^ " ") is_oneline_last_comment_exsits
        else
          (* 最後の要素なので区切り文字を入れなくてよい *)
          (* 最後の要素のあとにコメントがあった場合にフラグをtrueにする *)
          sub xs (buf ^ code_str) is_last_exists_after_comment
    )
  in
  let (is_multiline, online_code_str, is_oneline_last_comment_exsits) = sub lst "" false in
  if not is_multiline && String.length online_code_str < len_max ctx then
    (* 一行であることが確定している *)
    let v = Stack.create () in
    let () =
      match before_comments_format ctx before_comments with
      | Some(lst) -> append_to_stack v lst
      | None -> ()
    in
    match after_comment_opt with
    | Some(after_comment) -> (
      let comment = ctx.oneline_comment_format after_comment in
      let () = v |> Stack.push (online_code_str ^ " " ^ comment) in
      (stack_to_lst v, true)
    )
    | None -> (
      let () = v |> Stack.push online_code_str in
      (stack_to_lst v, is_oneline_last_comment_exsits)
    )
  else
    (* 複数行 *)
    let v = Stack.create () in
    let () =
      match before_comments_format ctx before_comments with
      | Some(lst) -> append_to_stack v lst
      | None -> ()
    in
    let rec sub lst =
      match lst with
      | [] -> ()
      | new_rule_with_comment::xs -> (
        let (code_lst, is_exsits_after_comment) =
          code_format_sub (ctx |> increment_depth |> set_list_join_str (Some(join))) new_rule_with_comment
        in
        let is_last = List.length xs == 0 in (* 全体の最後 *)
        let rec sub2 lst =
          match lst with
          | [] -> ()
          | code_str::xs -> (
            let is_local_last = List.length xs == 0 in
            let () =
              if (not is_last) && is_local_last then
                (* 区切り文字を入れたい *)
                if is_exsits_after_comment then
                  (* コメント絡みの前処理段階で既にjoin文字列が挿入されている *)
                  v |> Stack.push (tab ^ code_str)
                else
                  v |> Stack.push (tab ^ code_str ^ join)
              else
                (* 最後の要素なので区切り文字を入れなくてよい *)
                v |> Stack.push (tab ^ code_str)
            in
            sub2 xs
          )
        in
        let () = sub2 code_lst in
        sub xs
      )
    in
    let () = sub lst in
    match after_comment_opt with
    | Some(after_comment) -> let () = v |> Stack.push after_comment in (stack_to_lst v, true)
    | None -> (stack_to_lst v, false)


(* 貪欲法でトークンの行分割を行う *)
and break_token_column
  (ctx:context)
  (before_comments: string list)
  (lst : (rule_with_comment * column_config) list)
  (after_comment_opt : string option)
  : (string list * bool) =
  let v = Stack.create () in
  let () =
    match before_comments_format ctx before_comments with
    | Some(lst) -> append_to_stack v lst
    | None -> ()
  in
  let is_last_exists_after_comment_global = ref true in
  let rec sub buf1 buf1_after_spaces buf2 buf2_after_spaces lst =
    match lst with
    | (rule_with_comment, config)::xs -> (
      let (str_lst, is_last_exists_after_comment) =
        code_format_sub (ctx |> set_list_join_str(None)) rule_with_comment
      in
      is_last_exists_after_comment_global := is_last_exists_after_comment;
      if List.length str_lst > 1 then
        let () =
          if not (is_string_empty buf1) then
            v |> Stack.push (buf1 ^ str_repeat " " buf1_after_spaces ^ buf2)
        in
        let () = append_to_stack v str_lst in
        if is_last_exists_after_comment then
          sub "" 0 "" 0 xs
        else
          let last_code = Stack.pop v in
          sub last_code (config.space_size |> option_unwrap_or 1) "" 0 xs
      else
        let buf1_len = String.length buf1 in
        let buf2_len = String.length buf2 in
        (* 一行であることが保証されている *)
        let code_str = str_lst |> lst_join "" in
        let code_str_len = String.length code_str in
        if buf1_len + buf1_after_spaces + buf2_len + buf2_after_spaces + code_str_len <= (len_max ctx) then
          (* 行長が制限を超えなかったため、そのまま一行にする *)
          match config.is_break with
          | Some(true) -> (
            (*
              そのあとで絶対に改行
              色々値を更新する
            *)
            let () =
              if is_string_empty buf1 then
                v |> Stack.push code_str
              else
                v |> Stack.push (
                  buf1 ^
                  str_repeat " " buf1_after_spaces ^
                  buf2 ^
                  str_repeat " " buf2_after_spaces ^
                  code_str
                )
            in
            sub "" 0 "" 0 xs
          )
          | Some(false) -> (
            (* 改行不可ポイント *)
            if is_string_empty buf1 then
              sub code_str (config.space_size |> option_unwrap_or 1) "" 0 xs
            else
              sub buf1 buf1_after_spaces (buf2 ^ code_str) (config.space_size |> option_unwrap_or 1) xs
          )
          | None -> (
            (*
              改行可能ポイント
              全てbuf1に入れてbuf2を初期化
            *)
            let buf1 = if is_string_empty buf1 then
              buf1 ^ buf2
            else
              buf1 ^ str_repeat " " buf1_after_spaces ^ buf2
            in
            let buf1 = if is_string_empty buf2 then
              buf1 ^ code_str
            else
              buf1 ^ str_repeat " " buf2_after_spaces ^ code_str
            in
            sub buf1 (config.space_size |> option_unwrap_or 1) "" 0 xs
          )
        else
          (* 複数に改行しなければならない *)
          if buf2_len == 0 then
            (* 直前が改行可能ポイントである *)
            let () =
              if is_string_empty buf1 then
                v |> Stack.push buf2
              else
                v |> Stack.push (buf1 ^ str_repeat " " buf1_after_spaces ^ buf2)
            in
            sub code_str (config.space_size |> option_unwrap_or 1) "" 0 xs
          else
            (* 直前が改行不可ポイントである *)
            if buf2_len + buf2_after_spaces + code_str_len <= len_max ctx then
              let () =
                if not (is_string_empty buf1) then
                  v |> Stack.push buf1
              in
              let () = v |> Stack.push (buf2 ^ str_repeat " " buf2_after_spaces ^ code_str) in
              sub "" 0 "" 0 xs
            else
              (*
                buf2とcode_strをくっつけると行数オーバーする
                はみ出す量がより少ない方を取る
              *)
              if buf1_len + buf1_after_spaces + buf2_len
                > buf2_len + buf2_after_spaces + code_str_len then
                (* buf1とbuf2をくっつけた方がはみ出しが少ない *)
                let () = if not (is_string_empty buf1) then
                  v |> Stack.push (buf1 ^ str_repeat " " buf1_after_spaces ^ buf2)
                in
                sub code_str (config.space_size |> option_unwrap_or 1) "" 0 xs
              else
                (* buf2とcode_strをくっつけた方がはみ出しが少ない *)
                let () = if not (is_string_empty buf1) then
                  v |> Stack.push buf1
                in
                let () = v |> Stack.push (buf2 ^ str_repeat " " buf2_after_spaces ^ code_str) in
                sub "" 0 "" 0 xs
    )
    | [] -> (
      (* ループを抜け出す *)
      if is_string_empty buf2 then
        v |> Stack.push buf1
      else
        v |> Stack.push (buf1 ^ str_repeat " " buf1_after_spaces ^ buf2)
    )
  in
  let () = sub "" 0 "" 0 lst in
  match after_comment_opt with
  | Some(after_comment) ->
    let () =
      if not !is_last_exists_after_comment_global then
        if Stack.is_empty v then
          let last_code = Stack.pop v in
          let comment = ctx.oneline_comment_format after_comment in
          let code = last_code ^ " " ^ comment in
          Stack.push code v
        else
          v |> Stack.push (ctx.oneline_comment_format after_comment)
      else
        v |> Stack.push (ctx.oneline_comment_format after_comment)
    in
    (stack_to_lst v, true)
  | None -> (stack_to_lst v, false)


let code_format (ctx:context) (rule_with_comment:rule_with_comment) : string =
  let (lst, _) = code_format_sub ctx rule_with_comment in
  Types.lst_join ctx.break_str lst
