declare
    var x;
    var y;
    var z;
    func f x y (
        if (x / 2 == (x + 1) / 2) then (
            return := "even"
        ) else (
            return := "odd"
        )
    )
    func g x y (
        if (x / 2 == (x + 1) / 2) then (
            return := 0
        ) else (
            return := 1
        )
    )
end declare (
    x := -1;
    while x < 5 do (
        run f x;
        print y;
        run g x;
        print y;
        x := x + 1
    )
)