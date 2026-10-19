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
}

expect Fmt.fixed(2)(12.3) == "12.30"
expect  Fmt.fixed(2)(12.3456) == "12.35"
expect  Fmt.fixed(2)(-12.3456) == "-12.35"
expect Fmt.pad_left(10, Fmt.fixed(2))(12.3456) == "     12.35"
expect Fmt.pad_left(3, Fmt.fixed(2))(12.3456) == "12.35"

