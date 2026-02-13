let divsum n =
  let total = ref 0L in
  for i = 1 to n do
    let s = ref 0L in
    for j = 1 to i do
      if i mod j = 0 then s := Int64.add !s (Int64.of_int j)
    done;
    total := Int64.add !total !s
  done;
  !total

let () = Printf.printf "%Ld\n" (divsum 10000)
