import unicode.Grapheme

Fmt := {}.{
    fixed : U64 -> (Dec -> Str)
    fixed = |dp| {
        p = 10.Dec.pow(dp.to_dec())
        |n|
            match (n * p).round().div_by(p).to_str().split_on(".") {
                [i, f] if Grapheme.slices(f).len() < dp =>
                    "${i}.${f}${"0".repeat(dp - Grapheme.slices(f).len())}"
                [i, ..] if dp == 0 => i
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

    pad_right : U64, (a -> Str) -> (a -> Str)
    pad_right = |width, fn|
        |v|
            match fn(v) {
                s if Grapheme.slices(s).len() < width =>
                    "${s}${" ".repeat(width - Grapheme.slices(s).len())}"
                s => s
            }

    pad_center : U64, (a -> Str) -> (a -> Str)
    pad_center = |width, fn|
        |v|
            match fn(v) {
                s if Grapheme.slices(s).len() < width => {
                    p = width - Grapheme.slices(s).len()
                    "${" ".repeat(p.div_ceil_by(2))}${s}${" ".repeat(p.div_floor_by(2))}"
                }
                s => s
            }

    pad_left_with : U64, Str, (a -> Str) -> (a -> Str)
    pad_left_with = |width, pad, fn|
        |v| {
            s = fn(v)
            gs = Grapheme.slices(s)
            if gs.len() < width {
                gp = Grapheme.slices(pad)
                gp.repeat((width - gs.len()).div_ceil_by(gp.len()))
                    .join()
                    .concat(gs)
                    .take_last(width)
                    |> Str.join_with("")
            }
            else
                s
        }

    pad_right_with : U64, Str, (a -> Str) -> (a -> Str)
    pad_right_with = |width, pad, fn|
        |v| {
            s = fn(v)
            gs = Grapheme.slices(s)
            if gs.len() < width {
                gp = Grapheme.slices(pad)
                gs.concat(gp.repeat((width - gs.len()).div_ceil_by(gp.len()))
                            .join())
                    .take_first(width)
                    |> Str.join_with("")
            }
            else
                s
        }

    int : a -> Str where [ a.to_str : a -> Str, a.shl_wrap : a, U8 -> a, a.bitwise_and : a, a -> a ] # a should be an integer type
    int = |n| n.to_str()

    map: (b -> a), (a -> Str) -> (b -> Str)
    map = |fn, fmt|
        |v| v |> fn |> fmt

    sign : SignStyle, (a -> Str) -> (a -> Str)
    sign = |style, fn|
        |v| {
            vs = fn(v)
            if vs.starts_with("-")
                "${style.neg_prefix}${vs.drop_prefix("-")}${style.neg_suffix}"
            else
                "${style.pos_prefix}${vs.drop_prefix("-")}${style.pos_suffix}"
        }

    SignStyle := {
        pos_prefix: Str ?? "",
        pos_suffix: Str ?? "",
        neg_prefix: Str ?? "",
        neg_suffix: Str ?? ""
    }

    neg_only = SignStyle.{ neg_prefix: "-" }
    always = SignStyle.{ neg_prefix: "-", pos_prefix: "+" }
    accounting = SignStyle.{ neg_prefix: "(", neg_suffix: ")", pos_prefix: " ", pos_suffix: " " }

    thousands : (a -> Str) -> (a -> Str)
    thousands = |fn|
        on_magnitude(|m|
            match m.split_on(".") {
                [i, .. as rest] => {
                    i1 = Grapheme.slices(i)
                        .rev()
                        .chunks_of(3)
                        .map(|ch| ch.rev() |> Str.join_with(""))
                        .intersperse(",")
                        .rev()
                        |> Str.join_with("")
                    if rest.is_empty()
                        i1
                    else
                        "${i1}.${Str.join_with(rest, "")}"
                }
                [] => m
            }, fn)

    NumStyle := {
        dp: U64, 
        sign: SignStyle ?? neg_only,
        group:  Bool ?? False,
        width: U64 ?? 0,
        align: [Left, Right, Center] ?? Right
    }

    num : NumStyle -> (Dec -> Str)
    num = |spec| {
        n = Fmt.sign(spec.sign,
            (if spec.group Fmt.thousands else |i| i)(Fmt.fixed(spec.dp)))
        fmt = match spec.align {
            Right => Fmt.pad_left(spec.width, n)
            Left => Fmt.pad_right(spec.width, n)
            Center => Fmt.pad_center(spec.width, n)
        }
        |v| fmt(v)
    }

}

on_magnitude : (Str -> Str), (a -> Str) -> (a -> Str)
on_magnitude = |fmt, fn|
    |v| {
        vs = fn(v)
        if vs.starts_with("-")
            "-${vs.drop_prefix("-") |> fmt}"
        else
            fmt(vs)
    }

expect Fmt.fixed(2)(12.3) == "12.30"
expect Fmt.fixed(0)(12.3) == "12"
expect Fmt.fixed(2)(12.3456) == "12.35"
expect Fmt.fixed(2)(-12.3456) == "-12.35"
expect Fmt.fixed(2)(12.Dec) == "12.00"
expect Fmt.fixed(2)(12) == "12.00"
expect Fmt.fixed(0)(12.5) == "13"
expect Fmt.fixed(0)(12.4) == "12"
expect Fmt.fixed(2)(-0.001) == "0.00"
expect Fmt.pad_left(10, Fmt.fixed(2))(12.3456) == "     12.35"
expect Fmt.pad_left(3, Fmt.fixed(2))(12.3456) == "12.35"
expect Fmt.int(123) == "123"
expect Fmt.pad_left(10, Fmt.int)(1234) == "      1234"
expect Fmt.pad_left_with(10, "#", Fmt.int)(1234) == "######1234"
expect Fmt.pad_left_with(11, "|#", Fmt.int)(1234) == "#|#|#|#1234"
expect Fmt.pad_left_with(7, "0", Fmt.int)(-1234) == "00-1234" # padding is external to the number representation
expect Fmt.pad_right(10, Fmt.int)(1234) == "1234      "
expect Fmt.pad_right_with(10, "#", Fmt.int)(1234) == "1234######"
expect Fmt.pad_right_with(11, "|#", Fmt.int)(1234) == "1234|#|#|#|"
expect Fmt.pad_center(10, Fmt.int)(1234) == "   1234   "
expect Fmt.pad_center(11, Fmt.int)(1234) == "    1234   "

expect {
    Item : { price: Dec }
    i = { price: 12.5 }
    Fmt.map(|p| p.price, Fmt.fixed(2))(i) == "12.50"
}

expect Fmt.sign(Fmt.neg_only, Fmt.int)(12) == "12"
expect Fmt.sign(Fmt.neg_only, Fmt.int)(-12) == "-12"
expect Fmt.sign(Fmt.always, Fmt.int)(12) == "+12"
expect Fmt.sign(Fmt.always, Fmt.int)(-12) == "-12"
expect Fmt.sign(Fmt.accounting, Fmt.int)(12) == " 12 "
expect Fmt.sign(Fmt.accounting, Fmt.int)(-12) == "(12)"

expect Fmt.thousands(Fmt.int)(12345) == "12,345"
expect Fmt.thousands(Fmt.int)(-12345) == "-12,345"
expect Fmt.thousands(Fmt.fixed(2))(12345.6789) == "12,345.68"
expect Fmt.thousands(Fmt.fixed(2))(-12345.6789) == "-12,345.68"

expect Fmt.num({dp: 3, group: True, sign: Fmt.always, width: 12, align: Right})(1234.5678) == "  +1,234.568"
expect Fmt.num({dp: 3, group: True, width: 12, align: Right})(1234.5678) == "   1,234.568"
expect Fmt.num({dp: 3, group: True, width: 12, align: Left})(-1234.5678) == "-1,234.568  "
expect Fmt.num({dp: 3})(1234.5678) == "1234.568"
expect Fmt.num({dp: 2, sign: Fmt.accounting })(1234.5678) == " 1234.57 "
expect Fmt.num({dp: 2, sign: Fmt.accounting })(-1234.5678) == "(1234.57)"
expect Fmt.num({dp: 2, sign: Fmt.accounting, group: True })(-1234.5) == "(1,234.50)"
expect Fmt.num({dp: 2, sign: Fmt.accounting, group: True })(1234.5) == " 1,234.50 "
expect Fmt.num({dp: 2, width: 10, align: Center})(1234.5678) == "  1234.57 "
