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

    pad : PadStyle, (a -> Str) -> (a -> Str)
    pad = |spec, fn| {
        fs = Grapheme.slices(spec.fill)
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
                    Center =>
                        fs.repeat(pc.div_ceil_by(2))
                            .append(gs)
                            .join()
                            .concat(fs.repeat(pc.div_floor_by(2)).join())
                            .take_first(spec.width)
                            |> Str.join_with("")
                }
            }
        }
    }

    str : Str -> Str
    str = |s| s

    int : a -> Str where [ a.to_str : a -> Str, a.shl_wrap : a, U8 -> a, a.bitwise_and : a, a -> a ] # a should be an integer type
    int = |n| n.to_str()

    map : (b -> a), (a -> Str) -> (b -> Str)
    map = |fn, fmt|
        |v| v |> fn |> fmt

    sign : SignStyle, (a -> Str) -> (a -> Str)
    sign = |style, fn|
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

    separators : SeparatorStyle, (a -> Str) -> (a -> Str)
    separators = |spec, fn|
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
        n = Fmt.sign(spec.sign, Fmt.separators(spec.separators, Fmt.fixed(spec.dp)))
        fmt = pad({ width: spec.width, align: spec.align }, n)
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
        n = Fmt.sign(spec.sign, Fmt.separators(spec.separators, Fmt.int))
        fmt = pad({ width: spec.width, align: spec.align }, n)
        |v| fmt(v)
    }

    prefix : Str, (a -> Str) -> (a -> Str)
    prefix = |p, fn| on_magnitude(|m| "${p}${m}", fn)


    gbp = Fmt.prefix("£", Fmt.separators(Fmt.english, Fmt.fixed(2)))

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

expect 12.3 |> Fmt.fixed(2) == "12.30"
expect Fmt.fixed(0)(12.3) == "12"
expect Fmt.fixed(2)(12.3456) == "12.35"
expect Fmt.fixed(2)(-12.3456) == "-12.35"
expect Fmt.fixed(2)(12.Dec) == "12.00"
expect Fmt.fixed(2)(12) == "12.00"
expect Fmt.fixed(0)(12.5) == "13"
expect Fmt.fixed(0)(12.4) == "12"
expect Fmt.fixed(2)(-0.001) == "0.00"
expect Fmt.pad({ width: 10, align: Right }, Fmt.fixed(2))(12.3456) == "     12.35"
expect Fmt.pad({ width: 3, align: Right }, Fmt.fixed(2))(12.3456) == "12.35"
expect Fmt.int(123) == "123"
expect Fmt.pad({ width: 10, align: Right }, Fmt.int)(1234) == "      1234"
expect Fmt.pad({ width: 10, align: Right, fill: "#" }, Fmt.int)(1234) == "######1234"
expect Fmt.pad({ width: 11, align: Right, fill: "|#" }, Fmt.int)(1234) == "#|#|#|#1234"
expect Fmt.pad({ width: 7, align: Right, fill: "0" }, Fmt.int)(-1234) == "00-1234" # padding is external to the number representation
expect Fmt.pad({ width: 10, align: Left }, Fmt.int)(1234) == "1234      "
expect Fmt.pad({ width: 10, fill: "#", align: Left }, Fmt.int)(1234) == "1234######"
expect Fmt.pad({ width: 11, fill: "|#", align: Left }, Fmt.int)(1234) == "1234|#|#|#|"
expect Fmt.pad({ width: 10, align: Center }, Fmt.int)(1234) == "   1234   "
expect Fmt.pad({ width: 11, align: Center }, Fmt.int)(1234) == "    1234   "

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

expect Fmt.separators({}, Fmt.int)(12345) == "12345"
expect Fmt.separators({}, Fmt.int)(-12345) == "-12345"
expect Fmt.separators({}, Fmt.fixed(2))(12345.6789) == "12345.68"
expect Fmt.separators({}, Fmt.fixed(2))(-12345.6789) == "-12345.68"

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
    ].map(|item| "${Fmt.pad({ width: 11 }, Fmt.str)(item.desc)}|${Fmt.num_int({width: 5})(item.quantity)} @ ${Fmt.num({dp: 2, sign: Fmt.accounting, width: 10})(item.price)} = ${Fmt.pad({ width: 10, align: Right }, Fmt.sign(Fmt.accounting, Fmt.gbp))(item.price * item.quantity.to_dec())}")
    expected = [
        "saucepan   |    3 @     15.25  =    £45.75 ",
        "fryingpan  |    2 @     20.25  =    £40.50 ",
        "discount   |    1 @    (50.00) =   (£50.00)",
    ]
    actual == expected
}

expect Fmt.sign(Fmt.accounting, Fmt.prefix("£", Fmt.separators(Fmt.english, Fmt.fixed(2))))(1202.3) == " £1,202.30 "
expect Fmt.sign(Fmt.accounting, Fmt.prefix("£", Fmt.separators(Fmt.english, Fmt.fixed(2))))(-1202.3) == "(£1,202.30)"
