import unicode.Grapheme

Fmt := {}.{
    fixed : U64 -> (Dec -> Str)
    fixed = |dp| {
        p = 10.Dec.pow(dp.to_dec())
        |n|
            match (n * p).round().div_by(p).to_str().split_on(".") {
                [i, f] if Grapheme.slices(f).len() < dp =>
                    "${i}.${f}${"0".repeat(dp - Grapheme.slices(f).len())}"
                s => Str.join_with(s, ".")
            }
    }

    pad_left : U64, (a -> Str) -> (a -> Str)
    pad_left = |width, fn|
        |v|
            match fn(v) {
                s if Grapheme.slices(s).len() < width =>
                    "${" ".repeat(width - Grapheme.slices(s).len())}${s}"
                s => s
            }

    pad_left_with : U64, Str, (a -> Str) -> (a -> Str)
    pad_left_with = |width, pad, fn|
        |v|
            match fn(v) {
                s if Grapheme.slices(s).len() < width =>
                    Grapheme.slices(pad)
                        .repeat((width - Grapheme.slices(s).len()).div_ceil_by(Grapheme.slices(pad).len()))
                        .join()
                        .concat(Grapheme.slices(s))
                        .take_last(width)
                        |> Str.join_with("")
                s => s
            }

    int : a -> Str where [ a.to_str : a -> Str, a.is_even : a -> Bool ]
    int = |n| n.to_str()


}

expect Fmt.fixed(2)(12.3) == "12.30"
expect Fmt.fixed(2)(12.3456) == "12.35"
expect Fmt.fixed(2)(-12.3456) == "-12.35"
expect Fmt.pad_left(10, Fmt.fixed(2))(12.3456) == "     12.35"
expect Fmt.pad_left(3, Fmt.fixed(2))(12.3456) == "12.35"
expect Fmt.int(123) == "123"
expect Fmt.pad_left(10, Fmt.int)(1234) == "      1234"
expect Fmt.pad_left_with(10, "#", Fmt.int)(1234) == "######1234"
expect Fmt.pad_left_with(11, "|#", Fmt.int)(1234) == "#|#|#|#1234"

