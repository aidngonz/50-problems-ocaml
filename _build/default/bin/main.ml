let rec last list =
  match list with
  | [] -> None
  | [x] -> Some x
  | _ :: tl -> last tl
;;

let rec kth (list, index) =
  match list with
  | [] -> failwith "No items in list. Index out of bounds."
  | x :: tl -> 
      if index == 0 then x
      else kth (tl, index - 1)
  ;;

let rec len_h (list, total) =
  match list with
  | [] -> total
  | _ :: tl -> len_h (tl, total + 1)
;;

let len list =
  len_h (list, 0)
;;

let rec rev list =
  match list with
  | [] -> []
  | h :: tl -> (rev tl) @ [h]

let empty list =
  if list == [] then true
  else false

let () =
  let lastEx = ["a" ; "b" ; "c"] in
  match last lastEx with 
  | None -> Printf.printf "No items in list."
  | Some x -> Printf.printf "last item: %s\n" x;;

  let kthEx = ["a" ; "b" ; "c" ; "d" ; "e"] in
  Printf.printf "kth item: %s\n" (kth (kthEx, 2));;

  let lenEx = [1 ; 2 ; 3 ; 4 ; 5] in
  Printf.printf "len of list: %d\n" (len lenEx);;

  let reversed = rev [1; 2; 3; 4; 5] in
  Printf.printf "Reversed list: [%s]\n"
    (String.concat "; " (List.map string_of_int reversed));

  let emptyEx = empty ["a"] in
  Printf.printf "%b\n" emptyEx
;;
