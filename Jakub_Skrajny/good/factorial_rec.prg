declare
    var x;
    var nfactorial;
    func f x nfactorial (
        if (x == 1) then (
            return := 1
        ) else (
            run f (x - 1);
            return := (nfactorial * x)
        )
    )
end declare (
    run f (2 * 2);
    print nfactorial
)