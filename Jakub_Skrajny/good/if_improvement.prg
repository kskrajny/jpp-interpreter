declare
    var z;
    var y;
end declare (
    z := 1;
    y := 1;
    if ((z < 2) or (z == 3)) then (
        print "yes"
    ) else (
        print "no"
    );
    declare
        var z;
    end declare (
        z := "Local Z";
        print z
    );
    while ((z < 7) and (y == 1)) do (
        print z;
        z := z + 1;
        if (z == 5) then
            y := 2
        else
            skip
    )
)