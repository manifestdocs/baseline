let divsum n =
    let mutable total = 0L
    for i in 1..n do
        let mutable s = 0L
        for j in 1..i do
            if i % j = 0 then s <- s + int64 j
        total <- total + s
    total

printfn "%d" (divsum 10000)
