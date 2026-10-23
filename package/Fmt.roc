# SPDX-License-Identifier: UPL-1.0
# Copyright (c) 2026 Simon Redding

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

    PadStyle := { width: U64, align: [Left, Right, Center] ?? Left, fill: Str ?? " " }

    pad : (a -> Str), PadStyle -> (a -> Str)
    pad = |fn, spec| {
        fs = Grapheme.slices(if spec.fill.is_empty() " " else spec.fill)
        |v| {
            s = fn(v)
            gs = Grapheme.slices(s)
            if spec.width < gs.len()
                s
            else {
                pc = (spec.width - gs.len()).div_ceil_by(fs.len())
                match spec.align {
                    Left =>
                        fs.repeat(pc)
                            .prepend(gs)
                            .join()
                            .take_first(spec.width)
                            |> Str.join_with("")
                    Right =>
                        fs.repeat(pc)
                            .join()
                            .concat(gs)
                            .take_last(spec.width)
                            |> Str.join_with("")
                    Center => {
                        n = spec.width - gs.len()
                        l = n.div_ceil_by(2)
                        r = n - l
                        left = fs.repeat(l.div_ceil_by(fs.len())).join().take_last(l)
                        right = fs.repeat(r.div_ceil_by(fs.len())).join().take_first(r)
                        left.concat(gs)
                            .concat(right)
                            |> Str.join_with("")
                    }
                }
            }
        }
    }

    str : Str -> Str
    str = |s| s

    int : a -> Str where [ a.to_str : a -> Str, a.shl_wrap : a, U8 -> a, a.bitwise_and : a, a -> a ] # a should be an integer type
    int = |n| n.to_str()

    map : (a -> Str), (b -> a) -> (b -> Str)
    map = |fmt, fn|
        |v| v |> fn |> fmt

    sign : (a -> Str), SignStyle-> (a -> Str)
    sign = |fn, style|
        |v| {
            vs = fn(v)
            if vs.starts_with("-")
                "${style.neg_prefix}${vs.drop_prefix("-")}${style.neg_suffix}"
            else
                "${style.pos_prefix}${vs}${style.pos_suffix}"
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

    SeparatorStyle := {
        dec_sep: Str ?? ".",
        group_sep: Str ?? "",
        group_size: U64 ?? 3,
        first_group_size: U64 ?? 0,
    }

    english = SeparatorStyle.{ group_sep: "," }
    italian = SeparatorStyle.{ group_sep: ".", dec_sep: "," }
    french = SeparatorStyle.{ group_sep: " ", dec_sep: "," }
    indian = SeparatorStyle.{ group_sep: ",", group_size: 2, first_group_size: 3 }

    separators : (a -> Str), SeparatorStyle -> (a -> Str)
    separators = |fn, spec|
        on_magnitude(|m|
            match m.split_on(".") {
                [i, .. as rest] => {
                    digits = Grapheme.slices(i)
                        .rev()
                    i1 = digits
                        .take_first(spec.first_group_size)
                        |> List.single()
                        .concat(digits
                            .drop_first(spec.first_group_size)
                            .chunks_of(spec.group_size))
                        .drop_if(List.is_empty)
                        .map(|ch| ch.rev() |> Str.join_with(""))
                        .intersperse(spec.group_sep)
                        .rev()
                        |> Str.join_with("")
                    if rest.is_empty()
                        i1
                    else
                        "${i1}${spec.dec_sep}${Str.join_with(rest, "")}"
                }
                [] => m
            }, fn)

    DecStyle := {
        dp: U64,
        sign: SignStyle ?? neg_only,
        separators: SeparatorStyle ?? {},
        width: U64 ?? 0,
        align: [Left, Right, Center] ?? Right
    }

    num : DecStyle -> (Dec -> Str)
    num = |spec| {
        fmt = Fmt.fixed(spec.dp)
            |> Fmt.separators(spec.separators)
            |> Fmt.sign(spec.sign)
            |> Fmt.pad({ width: spec.width, align: spec.align })
        |v| fmt(v)
    }

    IntStyle := {
        sign: SignStyle ?? neg_only,
        separators: SeparatorStyle ?? {},
        width: U64 ?? 0,
        align: [Left, Right, Center] ?? Right
    }

    num_int : IntStyle -> (a -> Str)
        where [ a.to_str : a -> Str, a.shl_wrap : a, U8 -> a, a.bitwise_and : a, a -> a ] # a should be an integer type
    num_int = |spec| {
        fmt = Fmt.int
            |> Fmt.separators(spec.separators)
            |> Fmt.sign(spec.sign)
            |> Fmt.pad({ width: spec.width, align: spec.align })
        |v| fmt(v)
    }

    prefix : (a -> Str), Str -> (a -> Str)
    prefix = |fn, p| on_magnitude(|m| "${p}${m}", fn)


    gbp = Fmt.fixed(2) |> Fmt.separators(Fmt.english) |> Fmt.prefix("£")

    TruncStyle := {
        width: U64,
        keep: [Start, End, Middle, Both] ?? Start,
        ellipsis: Str ?? "…",
    }

    trunc : (a -> Str), TruncStyle -> (a -> Str)
    trunc = |fn, spec| {
        es = Grapheme.slices(spec.ellipsis)
        |v| {
            s = fn(v)
            ss = Grapheme.slices(s)
            if ss.len() <= spec.width
                s
            else if es.len() > spec.width
                es.take_first(spec.width) |> Str.join_with("")
            else
                match spec.keep {
                    Both =>{
                        l = (spec.width - es.len()).div_ceil_by(2)
                        r = spec.width - es.len() - l
                        ss.take_first(l)
                            .concat(es)
                            .concat(ss.take_last(r))
                            |> Str.join_with("")
                    }
                    End =>
                        es.concat(ss.take_last(spec.width - es.len()))
                            |> Str.join_with("")
                    Middle if spec.width >= 2 * es.len() => {
                        l = (ss.len() - spec.width).div_ceil_by(2)
                        r = ss.len() - spec.width - l
                        if r == 0
                            "${spec.ellipsis}${ss.drop_first(l + es.len()) |> Str.join_with("")}"
                        else
                            "${spec.ellipsis}${ss.drop_first(l + es.len()).drop_last(r + es.len()) |> Str.join_with("")}${spec.ellipsis}"
                    }
                    _ => # Start or Middle with overflowing ellipsis
                        ss.take_first(spec.width - es.len())
                            .concat(es)
                            |> Str.join_with("")
                }
        }
    }

    FitStyle := {
        width: U64,
        align: [Left, Right, Center] ?? Left,
        ellipsis: Str ?? "…",
    }

    fit : (a -> Str), FitStyle -> (a -> Str)
    fit = |fn, spec|
        fn |> Fmt.trunc({ width: spec.width, ellipsis: spec.ellipsis, keep:
            match spec.align {
                Left => Start
                Right => End
                Center => Middle
            } }) |> Fmt.pad({ width: spec.width, align: spec.align })

    OverflowStyle := {
        width: U64,
        mark: Str ?? "#",
    }

    overflow : (a -> Str), OverflowStyle -> (a -> Str)
    overflow = |fn, spec|
        |v| {
            s = fn(v)
            if width(s) > spec.width
                spec.mark
                    |> Grapheme.slices()
                    .repeat(spec.width)
                    .join()
                    .take_first(spec.width)
                    |> Str.join_with("")
            else
                s
        }

    width : Str -> U64
    width = |s| Grapheme.slices(s).len()
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
expect Fmt.fixed(0)(-12.5) == "-13"
expect Fmt.fixed(0)(12.4) == "12"
expect Fmt.fixed(0)(-12.4) == "-12"
expect Fmt.fixed(2)(-0.001) == "0.00"
expect (Fmt.fixed(2) |> Fmt.pad({ width: 10, align: Right }))(12.3456) == "     12.35"
expect (Fmt.fixed(2) |> Fmt.pad({ width: 3, align: Right }))(12.3456) == "12.35"
expect Fmt.int(123) == "123"
expect (Fmt.int |> Fmt.pad({ width: 10, align: Right }))(1234) == "      1234"
expect Fmt.pad(Fmt.int, { width: 10, align: Right, fill: "#" })(1234) == "######1234"
expect Fmt.pad(Fmt.int, { width: 10, align: Right, fill: "" })(1234) == "      1234"
expect Fmt.pad(Fmt.int, { width: 11, align: Right, fill: "|#" })(1234) == "#|#|#|#1234"
expect Fmt.pad(Fmt.int, { width: 7, align: Right, fill: "0" })(-1234) == "00-1234" # padding is external to the number representation
expect Fmt.pad(Fmt.int, { width: 10, align: Left })(1234) == "1234      "
expect Fmt.pad(Fmt.int, { width: 10, fill: "#", align: Left })(1234) == "1234######"
expect Fmt.pad(Fmt.int, { width: 11, fill: "|#", align: Left })(1234) == "1234|#|#|#|"
expect Fmt.pad(Fmt.int, { width: 10, align: Center })(1234) == "   1234   "
expect Fmt.pad(Fmt.int, { width: 11, align: Center })(1234) == "    1234   "
expect Fmt.pad(Fmt.int, { width: 5, fill: "|#", align: Center })(1234) == "#1234"

expect {
    Item : { price: Dec }
    i = { price: 12.5 }
    (Fmt.fixed(2) |> Fmt.map(|p| p.price))(i) == "12.50"
}

expect Fmt.sign(Fmt.int, Fmt.neg_only)(12) == "12"
expect Fmt.sign(Fmt.int, Fmt.neg_only)(-12) == "-12"
expect Fmt.sign(Fmt.int, Fmt.always)(12) == "+12"
expect Fmt.sign(Fmt.int, Fmt.always)(-12) == "-12"
expect Fmt.sign(Fmt.int, Fmt.accounting)(12) == " 12 "
expect (Fmt.int |> Fmt.sign(Fmt.accounting))(-12) == "(12)"

expect (Fmt.int |> Fmt.separators({}))(12345) == "12345"
expect Fmt.separators(Fmt.int, {})(-12345) == "-12345"
expect Fmt.separators(Fmt.fixed(2), {})(12345.6789) == "12345.68"
expect Fmt.separators(Fmt.fixed(2), {})(-12345.6789) == "-12345.68"

expect Fmt.num_int({separators: Fmt.english, sign: Fmt.always, width: 12, align: Right})(1234.U64) == "      +1,234"
expect Fmt.num_int({separators: Fmt.english, sign: Fmt.always, width: 12, align: Right})(1234.I64) == "      +1,234"
expect Fmt.num({dp: 3, separators: Fmt.english, sign: Fmt.always, width: 12, align: Right})(1234.5678) == "  +1,234.568"
expect Fmt.num({dp: 3, separators: Fmt.english, width: 12, align: Right})(1234.5678) == "   1,234.568"
expect Fmt.num({dp: 3, separators: Fmt.english, width: 12, align: Left})(-1234.5678) == "-1,234.568  "
expect Fmt.num({dp: 3, separators: Fmt.french, sign: Fmt.always, width: 12, align: Right})(1234.5678) == "  +1 234,568"
expect Fmt.num({dp: 3, separators: Fmt.french, width: 12, align: Right})(1234.5678) == "   1 234,568"
expect Fmt.num({dp: 3, separators: Fmt.french, width: 12, align: Left})(-1234.5678) == "-1 234,568  "
expect Fmt.num({dp: 3, separators: Fmt.italian, sign: Fmt.always, width: 12, align: Right})(1234.5678) == "  +1.234,568"
expect Fmt.num({dp: 3, separators: Fmt.italian, width: 12, align: Right})(1234.5678) == "   1.234,568"
expect Fmt.num({dp: 3, separators: Fmt.italian, width: 12, align: Left})(-1234.5678) == "-1.234,568  "
expect Fmt.num({dp: 0, separators: Fmt.indian})(123456789) == "12,34,56,789"
expect Fmt.num({dp: 3})(1234.5678) == "1234.568"
expect Fmt.num({dp: 2, sign: Fmt.accounting })(1234.5678) == " 1234.57 "
expect Fmt.num({dp: 2, sign: Fmt.accounting })(-1234.5678) == "(1234.57)"
expect Fmt.num({dp: 2, sign: Fmt.accounting, separators: { group_sep: "," } })(-1234.5) == "(1,234.50)"
expect Fmt.num({dp: 2, sign: Fmt.accounting, separators: { group_sep: "," } })(1234.5) == " 1,234.50 "
expect Fmt.num({dp: 2, width: 10, align: Center})(1234.5678) == "  1234.57 "
expect "${Fmt.num({dp: 2})(0.75432 * 100)}%" == "75.43%" # this is better than a Fmt.percent function, I think

expect {
    actual = [
        { desc: "saucepan", price: 15.25, quantity: 3 },
        { desc: "fryingpan", price: 20.25, quantity: 2 },
        { desc: "discount", price: -50, quantity: 1 },
        { desc: "a really long description", price: 0.5, quantity: 1_000_000 },
    ].map(|item| "${(Fmt.str |> Fmt.trunc({ width: 11 }) |> Fmt.pad({ width: 11 }))(item.desc)}|${(Fmt.num_int({width: 5})|> Fmt.overflow({ width: 5 }))(item.quantity) } @ ${Fmt.num({dp: 2, sign: Fmt.accounting, width: 10})(item.price)} = ${(Fmt.gbp |> Fmt.sign(Fmt.accounting) |> Fmt.overflow({ width: 10 }) |> Fmt.pad({ width: 10, align: Right }))(item.price * item.quantity.to_dec())}")
    expected = [
        "saucepan   |    3 @     15.25  =    £45.75 ",
        "fryingpan  |    2 @     20.25  =    £40.50 ",
        "discount   |    1 @    (50.00) =   (£50.00)",
        "a really l…|##### @      0.50  = ##########",
    ]
    actual == expected
}

expect (Fmt.fixed(2) |> Fmt.separators(Fmt.english) |> Fmt.prefix("£") |> Fmt.sign(Fmt.accounting))(1202.3) == " £1,202.30 "
expect (Fmt.fixed(2) |> Fmt.separators(Fmt.english) |> Fmt.prefix("£") |> Fmt.sign(Fmt.accounting))(-1202.3) == "(£1,202.30)"

expect Fmt.trunc(Fmt.str, { width: 20 })("abcdefg") == "abcdefg"
expect Fmt.trunc(Fmt.str, { width: 5 })("abcdefg") == "abcd…"
expect Fmt.trunc(Fmt.str, { width: 5, keep: End })("abcdefg") == "…defg"
expect Fmt.trunc(Fmt.str, { width: 5, keep: Middle })("abcdefg") == "…cde…"
expect Fmt.trunc(Fmt.str, { width: 5, keep: Both})("abcdefg") == "ab…fg"
expect Fmt.trunc(Fmt.str, { width: 6, keep: Both})("abcdefg") == "abc…fg"
expect Fmt.trunc(Fmt.str, { width: 1, keep: Middle })("abcdefg") == "…"
expect Fmt.trunc(Fmt.str, { width: 3, ellipsis: "----" })("abcdef") == "---"

expect (Fmt.str |> Fmt.fit({ width: 11 }))("saucepan") == "saucepan   "
expect (Fmt.str |> Fmt.fit({ width: 11, align: Right }))("saucepan") == "   saucepan"
expect (Fmt.str |> Fmt.fit({ width: 11, align: Center }))("saucepan") == "  saucepan "
expect (Fmt.str |> Fmt.fit({ width: 4 }))("saucepan") == "sau…"
expect (Fmt.str |> Fmt.fit({ width: 4, align: Right }))("saucepan") == "…pan"
expect (Fmt.str |> Fmt.fit({ width: 4, align: Center }))("saucepan") == "…ce…"
expect (Fmt.str |> Fmt.fit({ width: 7, align: Center }))("saucepan") == "…ucepan"
expect (Fmt.str |> Fmt.fit({ width: 8, align: Center }))("saucepan") == "saucepan"

expect (Fmt.int |> Fmt.overflow({ width: 10 }))(1_000_000) == "1000000"
expect (Fmt.int |> Fmt.overflow({ width: 5 }))(1_000_000) == "#####"
expect (Fmt.int |> Fmt.overflow({ width: 5, mark: "<>" }))(1_000_000) == "<><><"
