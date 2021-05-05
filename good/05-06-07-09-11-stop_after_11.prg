declare
    var x;
    var y;
    var z;
    func f x y
        if x < z then
            x := x + 2;
            run f x;
            return := y
        else
            return := x
end declare (
    x := 0;
    y := 0;
    z := 11;
    run f x;
    print y
)