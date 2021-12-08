
```scala
/*
acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
cdfeb fcadb cdfeb cd

        signals
        0 = ???
        1 = ab
        2 = ??
        3 = ??
        4 = eafb
        5 = ???
        6 = ???
        7 = dab
        8 = acedgfb
        9 = ???

        l=5 -> 2/3/5
        2 doesn't have (e or f) in signal
        3 doesn't have (f or e )

        cdfbe ->
        fbcad
        gcdfa

        l=6 -> 0/6/9
        0 doesn't have (e or f) in signal
        6 doesn't have (a or b) in signal
        9 doesn't have (c or g) in signal

        cefabd -> 9? doesn't have g, meaning g -> e, c -> g
        cdfgeb -> 6? doesn't have a, meaning a -> c, b -> f
        cagedb -> 0? doesn't have f, meaning f -> d, e -> b

        signal-to-display
        a -> c/f
        b -> c/f
        c -> e/g
        d -> a
        e -> b/d
        f -> b/d
        g -> e/g

        ->> after 6
        a -> c
        b -> c/f --- ???
        c -> e/g
        d -> a
        e -> b/d
        f -> b/d
        g -> e/g

        ->> after 0
        a -> c
        b -> f
        c -> e/g
        d -> a
        e -> b
        f -> d
        g -> e/g

        ->> after all
        a -> c
        b -> f
        c -> g
        d -> a
        e -> b
        f -> d
        g -> e

*/
```